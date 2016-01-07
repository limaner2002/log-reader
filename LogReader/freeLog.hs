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
    { nWatchers :: Int
    , fileMap :: a
    } deriving Show

data LogReaderF a b next
    = CreateLog LogKey next
    | Continue LogKey next
    | DeleteLog LogKey next
      deriving Functor

data DirWatcherF a b next
    = AddLog LogKey (LogFile a b) next
    | CloseLog FileName next
    | IncWatchers DirPath next
    | DecWatchers DirPath next
    | CloseDir DirPath next
    | OpenDir DirPath next
    -- | GetFile LogKey (Maybe (LogFile a b) -> next)
      deriving Functor

makeFree ''LogReaderF
makeFree ''DirWatcherF

type DirMap = Map DirPath (LogDir (FileMap AppianLogFile))
type DirWatcher = Free (DirWatcherF Int Text)
type LogReader = Free (LogReaderF Int Text)
type AppianLogFile = LogFile Int Text
type LogReaderM = StateT DirMap IO

runDirTest :: (MonadState DirMap m) => DirWatcher a -> m a
runDirTest = iterM run
    where
      run :: (MonadState DirMap m) => DirWatcherF Int Text (m a) -> m a
      run (OpenDir path n) = do
        let newLogDir = LogDir 0 mempty
        modify $ M.insert path newLogDir
        n
      run (IncWatchers path n) = do
        modify $ M.adjust (\dir ->
                               dir {nWatchers = (nWatchers dir) + 1}
                          ) path
        n
      run (DecWatchers path n) = do
        modify $ M.adjust (\dir ->
                               dir {nWatchers = (nWatchers dir) - 1}
                          ) path
        n
      run (AddLog (path, fName) newLogFile n) = do
        modify $ M.adjust (\dir ->
                               dir
                               { fileMap = M.insert fName newLogFile (fileMap dir)
                               }
                          ) path
        n
      -- run (GetFile (path, fName) f) = do
      --   mDir <- gets $ M.lookup path
      --   case mDir of
      --     Nothing -> f Nothing
      --     Just dir ->
      --         f logFile
      --       where
      --         logFile = M.lookup fName (fileMap dir)

runLogReader :: (MonadIO m, MonadState DirMap m) => LogReader a -> m a
runLogReader = iterM run
    where
      run :: (MonadIO m, MonadState DirMap m) => LogReaderF Int Text (m a) -> m a
      run (CreateLog path n) = do
        chan <- liftIO newTChanIO
        let newLogFile = LogFile 0 chan :: AppianLogFile
        runDirTest $ addLog path newLogFile
        n
      -- run (Continue (dir, fName)) next = do

program :: LogReaderM
program = do
  