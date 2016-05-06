{-# LANGUAGE QuasiQuotes     #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies    #-}
{-# LANGUAGE FlexibleInstances    #-}
module LogReader.Data
    ( Path.fromAbsDir
    , Path.Path
    , Path.Rel
    , Path.File
    , (</>)
    , module LogReader.Data
    ) where

import           Yesod hiding (loadConfig)
import ClassyPrelude.Yesod hiding (loadConfig, (</>))
import Data.Aeson.TH
import qualified Data.Char as C
import LogReader.Watcher (TLogDirMap)

import Control.SuiteSettings
import Path

data LogReader = LogReader TLogDirMap

data Settings = Settings
    { jbossPath :: Path Abs Dir
    , applicationPath :: Path Abs Dir
    , tmpPath :: Path Abs Dir
    }

data LogType = Application
             | JBoss
             | Tmp
               deriving (Show, Eq, Read)

type LogFilePath = Path Rel File

instance PathMultiPiece (Path Rel File) where
    fromPathMultiPiece pieces = (parseRelFile . unpack) $ intercalate "/" pieces

    toPathMultiPiece = splitElem '/' . pack . fromRelFile

instance Read LogFilePath where

-- We have a familiar analogue from mkYesod, with just one extra parameter.
-- We'll discuss that later.
mkYesodSubData "LogReader" [parseRoutes|
/list/#LogType LogFilesR GET -- POST
/view/#LogType/+LogFilePath LogSocketR GET
/download/#LogType/+LogFilePath LogDownloadR GET
/status WatcherStatusR GET
|]

data LogFiles = LogFiles
    { logFiles :: [Text]
    }
  deriving (Show, Eq)

$(deriveJSON defaultOptions{fieldLabelModifier = drop 0, constructorTagModifier = map C.toLower} ''LogFiles)

class GetPath a where
    getPath :: Settings -> a -> Path Abs Dir

instance GetPath LogType where
    getPath settings Application = applicationPath settings
    getPath settings JBoss = jbossPath settings
    getPath settings Tmp = tmpPath settings

instance PathPiece LogType where
    fromPathPiece = readMay
    toPathPiece = pack . show

loadSettings :: (MonadThrow m, MonadBaseControl IO m, MonadIO m) => FilePath -> m (Either Text Settings)
loadSettings path = do
  config <- loadConfig path
  let get :: (MonadIO m, Configured a) => Text -> ExceptT Text m a
      get key = getVal config $ "logSettings." <> key
      absPath txt = do
        eTxt <- get txt
        ePath <- tryAny $ parseAbsDir $ unpack (eTxt :: Text)
        case ePath of
          Left exception -> ExceptT $ return $ Left $ tshow exception
          Right v -> ExceptT $ return $ Right v

  runExceptT ( Settings
           <$> absPath "jbossPath"
           <*> absPath "applicationPath"
           <*> absPath "testPath"
             )

withSettings = runSettings . loadSettings

