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
    , LogReader.Watcher.writeChan
    , LogReader.Watcher.readChan
    ) where

import Prelude ()
import ClassyPrelude hiding (tryReadTChan, dupTChan)
import qualified ClassyPrelude as CP

import Control.Lens
import Data.Map.Lens
import qualified Data.Map as M
--import Control.Monad.State
import System.FSNotify
import Control.Concurrent (threadDelay)
import System.IO (withBinaryFile, hFileSize, hSeek, IOMode(..), SeekMode(..))
import System.FilePath (takeDirectory, takeFileName)
import Data.Conduit
import Data.Conduit.Combinators (sourceHandle)
import Control.Concurrent.Async (async)

newtype LogChannel = LogChannel (STM (TChan ChannelMessage))

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
    , _dirChan :: LogChannel
    }

instance Show LogDir where
    show (LogDir watchers logFile _) = "LogDir {_nWatchers = " <> show watchers
                                  <> ", _logFile = " <> show logFile

data DirPath = DirPath Text
    deriving (Ord, Eq, Show)
data FileName = FileName Text
    deriving (Ord, Eq, Show)
newtype LogKey = LogKey (DirPath, FileName)
type LogFileMap = M.Map FileName LogFile
type LogDirMap = M.Map DirPath LogDir
type TLogDirMap = TVar LogDirMap

data ChannelMessage
    = Ping
    | Closed LogKey
    | Data Text

delay :: Int
delay = truncate 30e6

nTries :: Int
nTries = 10

makeLenses ''LogFile
makeLenses ''LogDir

openFile :: LogKey -> LogDirMap -> LogDirMap
openFile (LogKey (dir, fName)) =  at dir %~ (addDirWatcher fName)

closeFile :: LogKey -> LogDirMap -> LogDirMap
closeFile (LogKey (dir, fName)) = at dir %~ (removeDirWatcher fName)

-- If the directory is already being watched, just increment the user
-- counter, else create a new LogDir. Note this only creates an entry
-- for the file in the LogFileMap and not a new file on the file
-- system.
addDirWatcher :: FileName -> Maybe LogDir -> Maybe LogDir
addDirWatcher fName (Just logDir) = Just $ (newFile fName) . (nWatchers +~ 1) $ logDir
addDirWatcher fName Nothing = Just ((newFile fName) . (nWatchers +~ 1) $ newDir)
    where
      newDir = LogDir 0 mempty (LogChannel newTChan)

-- If the directory has more than one person watching, just decrement
-- the user counter, else stops watching the directory
removeDirWatcher :: FileName -> Maybe LogDir -> Maybe LogDir
removeDirWatcher fName (Just (LogDir 1 _ _)) = Nothing
removeDirWatcher fName (Just (LogDir n f c)) = Just $ LogDir (n-1) newFile c
    where
      newFile = at fName %~ removeUser $ f
removeDirWatcher _ Nothing = Nothing

newFile fName = logFile %~ (at fName %~ addUser)

-- If the file is already being watched, just increment the user
-- counter, else create a new file. Note this only creates an entry
-- for the file in the LogFileMap and not a new file on the file
-- system.
addUser :: Maybe LogFile -> Maybe LogFile
addUser (Just logFile) = Just $ (watchers +~ 1) $ logFile
addUser Nothing = Just $ LogFile 0 1 (LogChannel newBroadcastTChan)

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

tailFile :: TLogDirMap -> LogKey -> IO (Maybe (LogChannel), Maybe (LogChannel))
tailFile tLogDirMap (LogKey (dir, fName)) = do
  logDirMap <- atomically $ readTVar tLogDirMap
  updateTLogDirMap (openFile $ LogKey (dir, fName)) tLogDirMap

  case (logDirMap ^.at dir) of
    Nothing -> do
        _ <- async (
               withManager $ \mgr -> do
                 _ <- watchDir
                        mgr
                        (getPath dir)
                        (const True)
                        (updateAction tLogDirMap)
                 moveToEndOfFile (LogKey (dir, fName)) tLogDirMap
                 mainLoop tLogDirMap dir 0
              )
        return ()
    Just _ -> return ()

  atomically $ do
    logDirMap <- readTVar tLogDirMap
    let mFileChan = fmap (dupTChan . (^. channel)) $ getLogFile (LogKey (dir, fName)) logDirMap
        mDirChan = fmap (^. dirChan) logDirMap ^.at dir
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
  let logKey = toLogKey path

  case getLogFile logKey logDirMap of
    Nothing -> do
      putStrLn "This file is not currently being watched so nothing will happen."
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
          -- sourceHandle handle $$ writeChannel (logFile ^. channel)
          updateTLogDirMap (setFilePosition logKey size) tLogDirMap

writeChannel :: STM (TChan Text) -> Consumer Text IO ()
writeChannel stmChan = do
  mContents <- await
  case mContents of
    Nothing -> return ()
    Just contents -> do
       atomically $ stmChan >>= \chan -> writeTChan chan contents
       writeChannel stmChan

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
toLogKey' dir fName = LogKey (DirPath dir, FileName fName)

fromLogKey :: LogKey -> FilePath
fromLogKey (LogKey (DirPath dir, FileName fName)) = unpack dir </> unpack fName

mainLoop :: TLogDirMap -> DirPath -> Int -> IO ()
mainLoop tLogDirMap dir try
    | try == nTries = return ()
    | otherwise = do
  threadDelay delay

  mAction <- atomically $ do
    logDirMap <- readTVar tLogDirMap
    case fmap (^. dirChan) logDirMap ^.at dir of
      Nothing -> return Nothing
      Just stmChan -> do
                 tryReadChan stmChan

  case mAction of
    Nothing -> mainLoop tLogDirMap dir (try + 1)
    Just (Closed logKey) ->
        atomically $ modifyTVar tLogDirMap (closeFile logKey)
    Just Ping -> mainLoop tLogDirMap dir 0
    _ -> do
      putStrLn "Received data for some reason?"
      return ()

--   logDirMap <- atomically $ readTVar tLogDirMap

--   case getLogFile logKey logDirMap of
--     Nothing -> return ()
--     Just logFile -> 
--         mResult <- atomically $ tryReadTChan (logFile ^. channel)
--         case mResult of
--           Nothing -> mainLoop logKey tLogDirMap (try + 1)
--           Closed -> closeFile logKey logDirMap
        
  -- logDir <- atomically $ readTVar tLogDir
  -- print logDir
  -- threadDelay delay
  -- putStrLn "Exiting. TLogDirMap is:"
  -- logDir <- atomically $ readTVar tLogDir
  -- print logDir

tryReadChan :: LogChannel -> STM (Maybe ChannelMessage)
tryReadChan (LogChannel stmChan) = stmChan >>= CP.tryReadTChan

dupTChan :: LogChannel -> LogChannel
dupTChan (LogChannel stmChan) = LogChannel $ stmChan >>= CP.dupTChan

test :: IO ()
test = do
  let dir = DirPath "/private/tmp"
      fName = FileName "server.log"

  logDirMap <- atomically $ newTVar mempty
  _ <- tailFile logDirMap (LogKey (dir, fName))
  return ()

writeChan :: LogChannel -> ChannelMessage -> IO ()
writeChan (LogChannel stmChan) stuff =
    atomically $ stmChan >>= \chan -> writeTChan chan stuff

readChan :: LogChannel -> IO ChannelMessage
readChan (LogChannel stmChan) = atomically $ stmChan >>= CP.readTChan