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

data DirPath = DirPath Text
    deriving (Ord, Eq, Show)
data FileName = FileName Text
    deriving (Ord, Eq, Show)
type FileMap a = Map FileName a

data LogFile a b = LogFile
    { position :: a
    , channel :: STM (TChan b)
    }

instance Show a => Show (LogFile a b) where
    show (LogFile pos _) =
        unwords $ ["LogFile with position", show pos]

data LogDir a = LogDir
    { nWatchers :: Int
    , fileMap :: a
    } deriving Show

data LogReaderF next
    = CreateLog (LogFile Int Text -> next)
    | Continue next
    | SendLog next
    | DeleteLog next
      deriving Functor

data DirWatcherF next
    = OpenLog DirPath FileName next
    | CloseLog FileName next
    | IncWatchers DirPath next
    | DecWatchers DirPath next
    | CloseDir DirPath next
    | OpenDir DirPath next
      deriving Functor

makeFree ''LogReaderF
makeFree ''DirWatcherF

type DirMap = Map DirPath (LogDir (FileMap AppianLogFile))
type DirWatcher = Free DirWatcherF
type LogReader = Free LogReaderF
type AppianLogFile = LogFile Int Text

runDirTest :: (MonadState DirMap m) => DirWatcher a -> m a
runDirTest = iterM run
    where
      run :: (MonadState DirMap m) => DirWatcherF (m a) -> m a
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
      run (OpenLog path fName n) = do
        newLogFile <- runLog createLog
        modify $ M.adjust (\dir ->
                               dir
                               { fileMap = M.insert fName newLogFile (fileMap dir)
                               }
                          ) path
        n

runLog :: (Monad m) => LogReader a -> m a
runLog = iterM run
    where
      run :: (Monad m) => LogReaderF (m a) -> m a
      run (CreateLog f) = do
        let newLogFile = LogFile 0 newTChan :: AppianLogFile
        f newLogFile