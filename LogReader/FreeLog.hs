{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE OverloadedStrings #-}

module FreeLog
    ( runLogReader
    , createLog
    , DirPath(..)
    , FileName(..)
    , LogReader
    , deleteLog
    , sendUpdates
    , module Control.Monad.State
    ) where

import Prelude ()
import ClassyPrelude hiding (FilePath)

import Control.Concurrent.STM.TChan
import Control.Monad.Free
import Control.Monad.Free.TH
import Data.Map (Map)
import qualified Data.Map as M
import Control.Monad.State
import Control.Monad

data DirPath = DirPath Text
    deriving (Ord, Eq, Show)
data FileName = FileName Text
    deriving (Ord, Eq, Show)
type FileMap a = Map FileName a
type LogKey = (DirPath, FileName)

data LogFile a b = LogFile
    { position :: a
    , channels :: TChan b
    , watchers :: Int
    }

instance Show a => Show (LogFile a b) where
    show (LogFile pos channels watchers) =
        unwords $ [ "LogFile with position", show pos
                  , "\nWith", show watchers, "people watching"
                  , "\nand", show (length channels), "channels"
                  ]

data LogDir a = LogDir
    { numWatchers :: Int
    , fileMap :: a
    } deriving Show

data LogReaderF a b next
    = CreateLog LogKey next
    | SendUpdates LogKey next
    | DeleteLog LogKey (TChan b) next
      deriving Functor

data DirWatcherF a b next
    = AddLog LogKey (LogFile a b) next
    | CloseLog LogKey (TChan b) next
    | IncWatchers LogKey next
    | DecWatchers DirPath next
    | CloseDir DirPath next
    | OpenDir DirPath next
    | GetWatchers DirPath (Maybe a -> next)
    | GetFile LogKey (Maybe (LogFile a b) -> next)
    | UpdateLog LogKey a next
      deriving Functor

makeFree ''LogReaderF
makeFree ''DirWatcherF

type DirMap = Map DirPath (LogDir (FileMap AppianLogFile))
type DirWatcher = Free (DirWatcherF Int Text)
type LogReader = Free (LogReaderF Int Text)
type AppianLogFile = LogFile Int Text
type LogReaderM = LogReader (StateT DirMap IO ())

runDir :: (MonadState DirMap m) => DirWatcher a -> m a
runDir = iterM run
    where
      run :: (MonadState DirMap m) => DirWatcherF Int Text (m a) -> m a
      run (OpenDir path n) = do
        mWatchers <- runDir $ getWatchers path
        case mWatchers of
          Nothing -> do
            let newLogDir = LogDir 0 mempty
            modify $ M.insert path newLogDir
          Just _ -> do
            return ()
        n
      run (IncWatchers (dirPath, fName) n) = do
        modify $ M.adjust (\dir ->
                               dir {numWatchers = (numWatchers dir) + 1}
                          ) dirPath
        n
      run (DecWatchers path n) = do
        modify $ M.adjust (\dir ->
                               dir {numWatchers = (numWatchers dir) - 1}
                          ) path
        mWatchers <- runDir $ getWatchers path
        case mWatchers of
          Just 0 -> modify $ M.delete path
          _ -> return ()
        n
      run (AddLog (path, fName) newLogFile n) = do
        modify $ M.adjust (\dir ->
                               dir
                               { fileMap = M.insert fName newLogFile (fileMap dir)
                               }
                          ) path
        runDir $ incWatchers path
        n
      run (GetWatchers path f) = do
        logDir <- gets $ M.lookup path
        let mNWatchers = fmap numWatchers logDir
        f mNWatchers
      run (CloseLog (path, fName) channel n) = do
        modify $ M.adjust (\dir ->
                               dir
                               { fileMap = M.delete fName (fileMap dir)
                               , channels = delete channel (channels dir)
                               }
                          ) path
        runDir $ decWatchers path
        n
      run (CloseDir dirPath n) = do
        modify $ M.delete dirPath
        n
      run (GetFile (dirPath, fName) f) = do
        logDir <- gets $ M.lookup dirPath
        let mFile = logDir >>= (\x -> M.lookup fName (fileMap x))
        f mFile
      run (UpdateLog (dirPath, fName) newPos n) = do
        modify $ M.adjust (\dir ->
                               dir
                               { fileMap =
                                     M.adjust
                                      (\file ->
                                           file
                                           { position = newPos
                                           }
                                      ) fName (fileMap dir)
                               }
                          ) dirPath

        n

runLogReader :: (MonadIO m, MonadState DirMap m) => LogReader a -> m a
runLogReader = iterM run
    where
      run :: (MonadIO m, MonadState DirMap m) => LogReaderF Int Text (m a) -> m a
      run (CreateLog (dir, fName) n) = do
        mFile <- runDir $ getFile (dir, fName)
        runDir $ openDir dir
        case mFile of
          Nothing -> do
                     chan <- liftIO newBroadcastTChan
                     let newLogFile = LogFile 0 [chan] 1
                     runDir $ addLog (dir, fName) newLogFile
          Just (LogFile pos channel watchers) -> do
                     runDir $ 
                     chan <- atomically $ cloneTChan $ unsafeHead channels
                     return $ LogFile pos (chan : channels) (watchers + 1)

        n
      run (DeleteLog path channel n) = do
        runDir $ closeLog path channel
        n
      run (SendUpdates logKey n) = do
        file <- runDir $ getFile logKey
        putStrLn $ unwords ["Sending updates from file", tshow file]
        runDir $ updateLog logKey 10
        n