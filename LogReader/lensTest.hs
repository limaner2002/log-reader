{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}

import Prelude ()
import ClassyPrelude

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

data LogFile = LogFile
    { _position :: Integer
    , _watchers :: Int
    , _channel :: STM (TChan Text)
    }

instance Show LogFile where
    show (LogFile position watchers _) = "LogFile {_position = " <> show position <> ", _watchers = " <> show watchers <> "}"

data LogDir = LogDir
    { _nWatchers :: Int
    , _logFile :: LogFileMap
    } deriving Show

data DirPath = DirPath Text
    deriving (Ord, Eq, Show)
data FileName = FileName Text
    deriving (Ord, Eq, Show)
type LogKey = (DirPath, FileName)
type LogFileMap = M.Map FileName LogFile
type LogDirMap = M.Map DirPath LogDir
type TLogDirMap = TVar LogDirMap

delay :: Int
delay = truncate 30e6

makeLenses ''LogFile
makeLenses ''LogDir

openFile :: LogKey -> LogDirMap -> LogDirMap
openFile (dir, fName) =  at dir %~ (addDirWatcher fName)

closeFile :: LogKey -> LogDirMap -> LogDirMap
closeFile (dir, fName) = at dir %~ (removeDirWatcher fName)

-- If the directory is already being watched, just increment the user
-- counter, else create a new LogDir. Note this only creates an entry
-- for the file in the LogFileMap and not a new file on the file
-- system.
addDirWatcher :: FileName -> Maybe LogDir -> Maybe LogDir
addDirWatcher fName (Just logDir) = Just $ (newFile fName) . (nWatchers +~ 1) $ logDir
addDirWatcher fName Nothing = Just ((newFile fName) . (nWatchers +~ 1) $ newDir)
    where
      newDir = LogDir 0 mempty

-- If the directory has more than one person watching, just decrement
-- the user counter, else stops watching the directory
removeDirWatcher :: FileName -> Maybe LogDir -> Maybe LogDir
removeDirWatcher fName (Just (LogDir 1 _)) = Nothing
removeDirWatcher fName (Just (LogDir n f)) = Just $ LogDir (n-1) newFile
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
addUser Nothing = Just $ LogFile 0 1 newBroadcastTChan

removeUser :: Maybe LogFile -> Maybe LogFile
removeUser (Just (LogFile _ 1 _)) = Nothing
removeUser (Just (LogFile pos n chan)) = Just $ LogFile pos (n-1) chan
removeUser Nothing = Nothing

-- Sets the current position of the file. If the file does is not
-- already being watched, this does nothing. See ix from Data.Lens.At
-- for more.
setFilePosition :: LogKey -> Integer -> LogDirMap -> LogDirMap
setFilePosition (dir, fName) pos = ix dir %~ (logFile %~ (ix fName %~ (position .~ pos)))

-- getChannel :: LogKey -> LogDirMap -> Maybe (STM (TChan Text))
-- getChannel (dir, fName) logDirMap = logFileMap >>= mChan
--     where
--       logFileMap = (^. logFile) <$> (^.at dir) logDirMap
--       mChan :: LogFileMap -> Maybe (STM (TChan Text))
--       mChan x = (^. channel) <$> (^.at fName) x

getLogFile :: LogKey -> LogDirMap -> Maybe LogFile
getLogFile (dir, fName) logDirMap = ((^.logFile) <$> (^.at dir) logDirMap) >>= (^.at fName)

sendFileChanges :: Maybe (STM (TChan Text)) -> Text -> IO ()
sendFileChanges (Just stmChan) changes = atomically go
    where
      go = stmChan >>= \chan -> writeTChan chan changes
sendFileChanges Nothing _ = return ()

tailFile :: TLogDirMap -> LogKey -> IO ()
tailFile tLogDirMap (dir, fName) = do
  logDirMap <- atomically $ readTVar tLogDirMap
  updateTLogDirMap (openFile (dir, fName)) tLogDirMap

  case (logDirMap ^.at dir) of
    Nothing ->
        withManager $ \mgr -> do
           watchDir
             mgr
             (getPath dir)
             (const True)
             (updateAction tLogDirMap)
           moveToEndOfFile (dir, fName) tLogDirMap
           mainLoop tLogDirMap
    Just _ -> return ()

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
          then
              hSeek handle AbsoluteSeek 0
          else
              hSeek handle AbsoluteSeek (logFile ^. position)
          sourceHandle handle $$ writeChannel
          updateTLogDirMap (setFilePosition logKey size) tLogDirMap

writeChannel :: Consumer Text IO ()
writeChannel = do
  mContents <- await
  case mContents of
    Nothing -> return ()
    Just contents -> do
       putStrLn contents
       writeChannel

updateTLogDirMap :: (LogDirMap -> LogDirMap) -> TLogDirMap -> IO ()
updateTLogDirMap f tLogDirMap =
    atomically $ do
      logDirMap <- readTVar tLogDirMap
      let newLogDirMap = f logDirMap
      writeTVar tLogDirMap newLogDirMap

toLogKey :: FilePath -> LogKey
toLogKey path = (dir, fName)
    where
      dir = DirPath $ pack $ takeDirectory path
      fName = FileName $ pack $ takeFileName path

fromLogKey :: LogKey -> FilePath
fromLogKey (DirPath dir, FileName fName) = unpack dir </> unpack fName

mainLoop :: TLogDirMap -> IO ()
mainLoop tLogDir = do
  logDir <- atomically $ readTVar tLogDir
  print logDir
  threadDelay delay
  putStrLn "Exiting. TLogDirMap is:"
  logDir <- atomically $ readTVar tLogDir
  print logDir

test :: IO ()
test = do
  let dir = DirPath "/private/tmp"
      fName = FileName "server.log"

  logDirMap <- atomically $ newTVar mempty
  tailFile logDirMap (dir, fName)
