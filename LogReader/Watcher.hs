{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}

module LogReader.Watcher
    ( toLogKey
    , toLogKey'
    , tailFile
    , LogKey
    , TLogDirMap
    , ChannelMessage (..)
    , LogChannel
    , DirChannel
    , LogReader.Watcher.writeDChan
    , LogReader.Watcher.readFChan
    , delay
    ) where

import Prelude ()
import ClassyPrelude hiding (tryReadTChan, dupTChan, writeChan)
import qualified ClassyPrelude as CP

import Control.Lens
import Data.Map.Lens
import qualified Data.Map as M
import System.FSNotify
import Control.Concurrent (threadDelay)
import System.IO (withBinaryFile, hFileSize, hSeek, IOMode(..), SeekMode(..))
import System.FilePath (takeDirectory, takeFileName)
import Data.Conduit
import Data.Conduit.Combinators (sourceHandle)
import Control.Concurrent.Async (async, race_)
import Data.Aeson hiding ((.=))
import qualified Data.Aeson as AE

newtype LogChannel = LogChannel (TChan ChannelMessage)
newtype DirChannel = DirChannel (TChan ChannelMessage)

data LogFile = LogFile
    { _position :: !Integer
    , _watchers :: !Int
    , _channel :: LogChannel
    }

instance Show LogFile where
    show (LogFile position watchers _) = "LogFile {_position = " <> show position
                                      <> ", _watchers = " <> show watchers <> "}"

data LogDir = LogDir
    { _nWatchers :: !Int
    , _logFile :: LogFileMap
    , _dirChan :: DirChannel
    }

instance Show LogDir where
    show (LogDir watchers logFile _) = "LogDir {_nWatchers = " <> show watchers
                                  <> ", _logFile = " <> show logFile

data DirPath = DirPath Text
    deriving (Ord, Eq, Show)
data FileName = FileName Text
    deriving (Ord, Eq, Show)
newtype LogKey = LogKey (DirPath, FileName)
    deriving Show

type LogFileMap = M.Map FileName LogFile
type LogDirMap = M.Map DirPath LogDir
type TLogDirMap = TVar LogDirMap

data ChannelMessage
    = Ping
    | Closed LogKey
    | Data Text
      deriving Show

instance ToJSON ChannelMessage where
    toJSON Ping = object["type" AE..= ("Ping" :: Text)]
    toJSON (Data contents) =
        object ["type" AE..= ("Data" :: Text), "contents" AE..= contents]

delay :: Int
delay = truncate 30e6

pollingInterval :: Int
pollingInterval = truncate 5e6

makeLenses ''LogFile
makeLenses ''LogDir

openFile :: LogKey -> DirChannel -> LogChannel -> LogDirMap -> LogDirMap
openFile (LogKey (dir, fName)) dirChan fileChan =  at dir %~ (addDirWatcher fName dirChan fileChan)

closeFile :: LogKey -> LogDirMap -> LogDirMap
closeFile (LogKey (dir, fName)) = at dir %~ (removeDirWatcher fName)

-- If the directory is already being watched, just increment the user
-- counter, else create a new LogDir. Note this only creates an entry
-- for the file in the LogFileMap and not a new file on the file
-- system.
addDirWatcher :: FileName -> DirChannel -> LogChannel -> Maybe LogDir -> Maybe LogDir
addDirWatcher fName _ fileChan (Just logDir) = Just $ (newFile fName fileChan) . (nWatchers +~ 1) $ logDir
addDirWatcher fName dirChan fileChan Nothing = Just ((newFile fName fileChan) . (nWatchers +~ 1) $ newDir)
    where
      newDir = LogDir 0 mempty dirChan

-- If the directory has more than one person watching, just decrement
-- the user counter, else stops watching the directory
removeDirWatcher :: FileName -> Maybe LogDir -> Maybe LogDir
removeDirWatcher fName (Just (LogDir 1 _ _)) = Nothing
removeDirWatcher fName (Just (LogDir n f c)) = Just $ LogDir (n-1) newFile c
    where
      newFile = at fName %~ removeUser $ f
removeDirWatcher _ Nothing = Nothing

newFile :: FileName -> LogChannel -> LogDir -> LogDir
newFile fName chan = logFile %~ (at fName %~ addUser chan)

-- If the file is already being watched, just increment the user
-- counter, else create a new file. Note this only creates an entry
-- for the file in the LogFileMap and not a new file on the file
-- system.
addUser :: LogChannel -> Maybe LogFile -> Maybe LogFile
addUser _ (Just logFile) = Just $ (watchers +~ 1) $ logFile
addUser chan Nothing = Just $ LogFile 0 1 chan

removeUser :: Maybe LogFile -> Maybe LogFile
removeUser (Just (LogFile _ 1 _)) = Nothing
removeUser (Just (LogFile pos n chan)) = Just $ LogFile pos (n-1) chan
removeUser Nothing = Nothing

-- Sets the current position of the file. If the file does is not
-- already being watched, this does nothing. See ix from Data.Lens.At
-- for more.
setFilePosition :: LogKey -> Integer -> LogDirMap -> LogDirMap
setFilePosition (LogKey (dir, fName)) pos = ix dir %~ (logFile %~ (ix fName %~ (position .~ pos)))

getLogFile :: LogKey -> LogDirMap -> Maybe LogFile
getLogFile (LogKey (dir, fName)) logDirMap = ((^.logFile) <$> (^.at dir) logDirMap) >>= (^.at fName)

sendFileChanges :: Maybe (STM (TChan Text)) -> Text -> IO ()
sendFileChanges (Just stmChan) changes = atomically go
    where
      go = stmChan >>= \chan -> writeTChan chan changes
sendFileChanges Nothing _ = return ()

tailFile :: TLogDirMap -> LogKey -> IO (Maybe (LogChannel), Maybe (DirChannel))
tailFile tLogDirMap (LogKey (dir, fName)) = do
  logDirMap <- atomically $ readTVar tLogDirMap
  fileChannel <- LogChannel <$> newTChanIO
  dirChannel <- DirChannel <$> newTChanIO
  updateTLogDirMap (openFile (LogKey (dir, fName)) dirChannel fileChannel) tLogDirMap

  case (logDirMap ^.at dir) of
    Nothing -> do
        let conf = defaultConfig { confUsePolling = True
                                 , confPollInterval = pollingInterval
                                 }
        _ <- async (
               withManagerConf conf $ \mgr -> do
                 _ <- watchDir
                        mgr
                        (getPath dir)
                        (const True)
                        (updateAction tLogDirMap)
                 moveToEndOfFile (LogKey (dir, fName)) tLogDirMap
                 race_
                   (forever $ do
                      threadDelay delay
                      atomically $ writeFChan fileChannel Ping
                   )
                   (mainLoop tLogDirMap dir)
              )
        return ()
    Just _ -> return ()

  atomically $ do
    logDirMap <- readTVar tLogDirMap
    let mDirChan = fmap (^. dirChan) $ logDirMap ^.at dir

    mFileChan <- sequence $ fmap (dupTChan . (^. channel)) $ getLogFile (LogKey (dir, fName)) logDirMap

    return (mFileChan, mDirChan)

moveToEndOfFile :: LogKey -> TLogDirMap -> IO ()
moveToEndOfFile logKey tLogDirMap =
    withBinaryFile path ReadMode $ \handle -> do
      size <- hFileSize handle
      updateTLogDirMap (setFilePosition logKey size) tLogDirMap
  where
    path = fromLogKey logKey

getPath :: DirPath -> FilePath
getPath (DirPath path) = unpack path

updateAction :: TLogDirMap -> Event -> IO ()
updateAction _ (Added _ _) = return ()
updateAction _ (Removed _ _) = return ()
updateAction tLogDirMap (Modified path _) = do
  logDirMap <- atomically $ readTVar tLogDirMap
  putStrLn $ "File " <> pack path <> " was modified."
  putStrLn $ "logDirMap " <> tshow logDirMap
  let logKey = toLogKey path

  case getLogFile logKey logDirMap of
    Nothing -> do
      putStrLn $ "The file " <> pack path <> " is not currently being watched so nothing will happen."
      return ()
    Just logFile -> do
      withBinaryFile path ReadMode $ \handle -> do
          size <- hFileSize handle
          if size < (logFile ^. position)
            then                  -- The file just rolled over or was
                                -- truncated. Start from the beginning
              hSeek handle AbsoluteSeek 0
            else                  -- Move to the old position and read
                                -- only the new data.
              hSeek handle AbsoluteSeek (logFile ^. position)
          sourceHandle handle $$ writeChannel (logFile ^. channel)
          updateTLogDirMap (setFilePosition logKey size) tLogDirMap

writeChannel :: LogChannel -> Consumer Text IO ()
writeChannel chan = do
  mContents <- await
  case mContents of
    Nothing -> return ()
    Just contents -> do
       putStrLn $ "Writing " <> contents <> " to channel"
       liftIO $ atomically $ writeFChan chan (Data contents)
       writeChannel chan

updateTLogDirMap :: (LogDirMap -> LogDirMap) -> TLogDirMap -> IO ()
updateTLogDirMap f tLogDirMap =
    atomically $ do
      logDirMap <- readTVar tLogDirMap
      let newLogDirMap = f logDirMap
      writeTVar tLogDirMap newLogDirMap

toLogKey :: FilePath -> LogKey
toLogKey path = LogKey (dir, fName)
    where
      dir = DirPath $ pack $ takeDirectory path
      fName = FileName $ pack $ takeFileName path

toLogKey' :: Text -> Text -> LogKey
toLogKey' dir fName = toLogKey path
    where
      path = unpack dir </> unpack fName

fromLogKey :: LogKey -> FilePath
fromLogKey (LogKey (DirPath dir, FileName fName)) = unpack dir </> unpack fName

mainLoop :: TLogDirMap -> DirPath -> IO ()
mainLoop tLogDirMap dir = do
  putStrLn "Entered main loop"
  mAction <- atomically $ do
    logDirMap <- readTVar tLogDirMap
    case fmap (^. dirChan) $ logDirMap ^.at dir of
      Nothing -> return Nothing
      Just chan -> do
        msg <- readDChan chan
        return $ Just msg

  case mAction of
    Nothing -> do
      putStrLn "Exiting main loop"
      return ()
    Just (Closed logKey) -> do
          atomically $ modifyTVar tLogDirMap (closeFile logKey)
          mainLoop tLogDirMap dir
    Just _ -> mainLoop tLogDirMap dir

tryReadFChan :: LogChannel -> STM (Maybe ChannelMessage)
tryReadFChan (LogChannel chan) = CP.tryReadTChan chan

tryReadDChan :: DirChannel -> STM (Maybe ChannelMessage)
tryReadDChan (DirChannel chan) = tryReadFChan $ LogChannel chan

dupTChan :: LogChannel -> STM (LogChannel)
dupTChan (LogChannel chan) = LogChannel <$> CP.dupTChan chan

test :: IO ()
test = do
  let path = "/private/tmp/server.log"

  tLogDirMap <- atomically $ newTVar mempty
  (Just rChan, Just wChan) <- tailFile tLogDirMap (toLogKey path)
  threadDelay 20000000

  logDirMap <- atomically $ readTVar tLogDirMap
  print logDirMap

  result <- atomically $ readFChan rChan
  print result

  putStrLn "Sending close"
  atomically $ writeDChan wChan $ Closed (toLogKey path)

  threadDelay 2000000
  logDirMap <- atomically $ readTVar tLogDirMap
  print logDirMap

writeFChan :: LogChannel -> ChannelMessage -> STM ()
writeFChan (LogChannel chan) stuff =
    writeTChan chan stuff

writeDChan :: DirChannel -> ChannelMessage -> STM ()
writeDChan (DirChannel chan) stuff =
    writeFChan (LogChannel chan) stuff

readFChan :: LogChannel -> STM ChannelMessage
readFChan (LogChannel chan) = CP.readTChan chan

readDChan :: DirChannel -> STM ChannelMessage
readDChan (DirChannel chan) = readFChan (LogChannel chan)