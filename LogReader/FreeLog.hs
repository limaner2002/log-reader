module FreeLog
    ( runLogReader
    , createLog
    , DirPath(..)
    , FileName(..)
    , deleteLog
    , module Control.Monad.State
    ) where

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveFunctor #-}

import Prelude ()
import ClassyPrelude.Yesod hiding (FilePath)

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
    , channel :: TChan b
    }

instance Show a => Show (LogFile a b) where
    show (LogFile pos _) =
        unwords $ ["LogFile with position", show pos]

data LogDir a = LogDir
    { numWatchers :: Int
    , fileMap :: a
    } deriving Show

data LogReaderF a b next
    = CreateLog LogKey next
    | Continue LogKey next
    | DeleteLog LogKey next
      deriving Functor

data DirWatcherF a b next
    = AddLog LogKey (LogFile a b) next
    | CloseLog LogKey next
    | IncWatchers DirPath next
    | DecWatchers DirPath next
    | CloseDir DirPath next
    | OpenDir DirPath next
    | GetWatchers DirPath (Maybe a -> next)
    -- | GetFile LogKey (Maybe (LogFile a b) -> next)
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
        let newLogDir = LogDir 0 mempty
        modify $ M.insert path newLogDir
        n
      run (IncWatchers path n) = do
        modify $ M.adjust (\dir ->
                               dir {numWatchers = (numWatchers dir) + 1}
                          ) path
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
      run (CloseLog (path, fName) n) = do
        modify $ M.adjust (\dir ->
                               dir
                               { fileMap = M.delete fName (fileMap dir)
                               }
                          ) path
        runDir $ decWatchers path
        n
      run (CloseDir dirPath n) = do
        modify $ M.delete dirPath
        n

runLogReader :: (MonadIO m, MonadState DirMap m) => LogReader a -> m a
runLogReader = iterM run
    where
      run :: (MonadIO m, MonadState DirMap m) => LogReaderF Int Text (m a) -> m a
      run (CreateLog (dir, fName) n) = do
        chan <- liftIO newTChanIO
        let newLogFile = LogFile 0 chan :: AppianLogFile
        runDir $ do
          mWatchers <- getWatchers dir
          case mWatchers of
            Nothing -> openDir dir
            Just _ -> return ()
          addLog (dir, fName) newLogFile
        n
      run (DeleteLog path n) = do
        runDir $ closeLog path
        n
