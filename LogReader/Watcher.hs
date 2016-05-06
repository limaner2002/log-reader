{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}

module LogReader.Watcher
    ( toLogKey
    , toLogKey'
    , fromLogKey
    , tailFile
    , LogKey
    , TLogDirMap
    , ChannelMessage (..)
    , LogChannel
    , DirChannel
    , LogReader.Watcher.writeDChan
    , LogReader.Watcher.readFChan
    , LogReader.Watcher.getPing
    , delay
    , showLogMap
    ) where

import Prelude ()
import ClassyPrelude hiding (tryReadTChan, dupTChan, writeChan, (</>))
import qualified ClassyPrelude as CP

import Control.Lens
import Data.Map.Lens
import qualified Data.Map as M
import System.FSNotify
-- import Control.Concurrent (threadDelay)
import System.IO (withBinaryFile, hFileSize, hSeek, IOMode(..), SeekMode(..))
--import System.FilePath (takeDirectory, takeFileName)
import Data.Conduit
import Data.Conduit.Combinators (sourceHandle)
import Control.Concurrent.Async (async, race_)
import Data.Aeson hiding ((.=))
import qualified Data.Aeson as AE
import System.IO (hFlush, stdout)
import Path

newtype LogChannel = LogChannel (TChan ChannelMessage)
    deriving Eq

data DirChannel = DirChannel (TChan ChannelMessage) (TChan ChannelMessage)
    deriving Eq

data LogFile = LogFile
    { _position :: !Integer
    , _watchers :: !Int
    , _channel :: LogChannel
    } deriving Eq

instance Show LogFile where
    show (LogFile position watchers _) = "LogFile {_position = " <> show position
                                      <> ", _watchers = " <> show watchers <> "}"

data LogDir = LogDir
    { _nWatchers :: !Int
    , _logFile :: LogFileMap
    , _dirChan :: DirChannel
    } deriving Eq

instance Show LogDir where
    show (LogDir watchers logFile _) = "LogDir {_nWatchers = " <> show watchers
                                  <> ", _logFile = " <> show logFile

newtype LogKey = LogKey (Path Abs Dir, Path Rel File)
    deriving (Show, Eq)

type LogFileMap = M.Map (Path Rel File) LogFile
type LogDirMap = M.Map (Path Abs Dir) LogDir

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
    toJSON (Closed logKey) =
    	object ["type" AE..= ("Closed" :: Text), "file" AE..= tshow logKey]

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
addDirWatcher :: Path Rel File -> DirChannel -> LogChannel -> Maybe LogDir -> Maybe LogDir
addDirWatcher fName _ fileChan (Just logDir) = Just $ (newFile fName fileChan) . (nWatchers +~ 1) $ logDir
addDirWatcher fName dirChan fileChan Nothing = Just ((newFile fName fileChan) . (nWatchers +~ 1) $ newDir)
    where
      newDir = LogDir 0 mempty dirChan

-- If the directory has more than one person watching, just decrement
-- the user counter, else stops watching the directory
removeDirWatcher :: Path Rel File -> Maybe LogDir -> Maybe LogDir
removeDirWatcher fName (Just (LogDir 1 _ _)) = Nothing
removeDirWatcher fName (Just (LogDir n f c)) = Just $ LogDir (n-1) newFile c
    where
      newFile = at fName %~ removeUser $ f
removeDirWatcher _ Nothing = Nothing

newFile :: Path Rel File -> LogChannel -> LogDir -> LogDir
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

tailFile :: TLogDirMap -> LogKey -> IO (Maybe (LogChannel), Maybe (DirChannel))
tailFile tLogDirMap (LogKey (dir, fName)) = do
  logDirMap <- atomically $ readTVar tLogDirMap
  fileChannel <- LogChannel <$> newBroadcastTChanIO -- Channel for the watcher to send file updates
  dirChannel <- DirChannel <$> newTChanIO <*> newBroadcastTChanIO

  updateTLogDirMap (openFile (LogKey (dir, fName)) dirChannel fileChannel) tLogDirMap

  case (logDirMap ^.at dir) of
    Nothing -> do
        let conf = defaultConfig { confUsePolling = True
                                 , confPollInterval = pollingInterval
                                 }
        _ <- async ( do
               withManagerConf conf $ \mgr -> do
                 _ <- watchDir
                        mgr
                        (fromAbsDir dir)
                        (const True)
                        (updateAction tLogDirMap)
                 moveToEndOfFile (LogKey (dir, fName)) tLogDirMap 0
                 race_
                   (forever $ do
                      threadDelay delay
                      atomically $ pingDChan dirChannel Ping
                   )
                   (mainLoop tLogDirMap dir)
              )
        return ()
    Just _ -> return ()

  atomically $ do
    logDirMap <- readTVar tLogDirMap

    mDirChan <- sequence $ fmap (dupDChan . (^. dirChan)) $ logDirMap ^.at dir
    mFileChan <- sequence $ fmap (dupFChan . (^. channel)) $ getLogFile (LogKey (dir, fName)) logDirMap

    return (mFileChan, mDirChan)

moveToEndOfFile :: LogKey -> TLogDirMap -> Integer -> IO ()
moveToEndOfFile logKey tLogDirMap offset =
    withBinaryFile path ReadMode $ \handle -> do
      size <- hFileSize handle
      updateTLogDirMap (setFilePosition logKey (position size)) tLogDirMap
  where
    path = fromLogKey logKey
    position size
        | offset >= size = size
        | otherwise = size - offset

updateAction :: TLogDirMap -> Event -> IO ()
updateAction _ (Added _ _) = return ()
updateAction _ (Removed _ _) = return ()
updateAction tLogDirMap (Modified path _) = do
  logKey <- toLogKey path
  sendContents logKey tLogDirMap

sendContents logKey tLogDirMap = do
  logDirMap <- atomically $ readTVar tLogDirMap
  case getLogFile logKey logDirMap of
    Nothing -> return ()
    Just logFile -> do
      withBinaryFile (fromLogKey logKey) ReadMode $ \handle -> do
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
       liftIO $ atomically $ writeFChan chan (Data contents)
       writeChannel chan

updateTLogDirMap :: (LogDirMap -> LogDirMap) -> TLogDirMap -> IO ()
updateTLogDirMap f tLogDirMap =
    atomically $ do
      logDirMap <- readTVar tLogDirMap
      let newLogDirMap = f logDirMap
      writeTVar tLogDirMap newLogDirMap

toLogKey :: (MonadThrow m) => FilePath -> m LogKey
toLogKey path = do
  absPath <- parseAbsFile path
  
  return $ toLogKey' absPath

toLogKey' :: Path Abs File -> LogKey
toLogKey' absPath = LogKey (parent absPath, filename absPath)

fromLogKey :: LogKey -> FilePath
fromLogKey (LogKey (dir, fName)) = fromAbsFile $ dir </> fName

mainLoop :: TLogDirMap -> Path Abs Dir -> IO ()
mainLoop tLogDirMap dir = do
  mAction <- atomically $ do
    logDirMap <- readTVar tLogDirMap
    case fmap (^. dirChan) $ logDirMap ^.at dir of
      Nothing -> return Nothing
      Just chan -> do
        msg <- readDChan chan
        return $ Just msg

  case mAction of
    Nothing -> return ()
    Just (Closed logKey) -> do
          atomically $ modifyTVar tLogDirMap (closeFile logKey)
          mainLoop tLogDirMap dir
    Just _ -> mainLoop tLogDirMap dir

tryReadFChan :: LogChannel -> STM (Maybe ChannelMessage)
tryReadFChan (LogChannel chan) = CP.tryReadTChan chan

tryReadDChan :: DirChannel -> STM (Maybe ChannelMessage)
tryReadDChan (DirChannel _ chan) = tryReadFChan $ LogChannel chan

dupFChan :: LogChannel -> STM (LogChannel)
dupFChan (LogChannel chan) = LogChannel <$> CP.dupTChan chan

dupDChan :: DirChannel -> STM (DirChannel)
dupDChan (DirChannel wChan bChan) = DirChannel wChan <$> CP.dupTChan bChan

writeFChan :: LogChannel -> ChannelMessage -> STM ()
writeFChan (LogChannel chan) stuff =
    writeTChan chan stuff

pingDChan :: DirChannel -> ChannelMessage -> STM ()
pingDChan (DirChannel _ bChan) stuff =
    writeFChan (LogChannel bChan) stuff

getPing :: DirChannel -> STM ChannelMessage
getPing (DirChannel _ rChan) = CP.readTChan rChan

writeDChan :: DirChannel -> ChannelMessage -> STM ()
writeDChan (DirChannel chan _) stuff =
    writeFChan (LogChannel chan) stuff

readFChan :: LogChannel -> STM ChannelMessage
readFChan (LogChannel chan) = CP.readTChan chan

readDChan :: DirChannel -> STM ChannelMessage
readDChan (DirChannel chan _) = readFChan $ LogChannel chan

showLogMap :: TLogDirMap -> IO Text
showLogMap tLogDirMap = do
  logDirMap <- atomically $ readTVar tLogDirMap
  return $ tshow logDirMap

test :: IO ()
test = do
  let path = "/private/tmp/server.log"

  logKey <- toLogKey path
  tLogDirMap <- atomically $ newTVar mempty
  (Just rChan, Just wChan) <- tailFile tLogDirMap logKey
  threadDelay $ truncate 20e6

  logDirMap <- atomically $ readTVar tLogDirMap
  print logDirMap

  result <- atomically $ readFChan rChan
  print result

  putStrLn "Sending close"
  atomically $ writeDChan wChan $ Closed logKey

  threadDelay $ truncate 20e6
  logDirMap <- atomically $ readTVar tLogDirMap
  print logDirMap

