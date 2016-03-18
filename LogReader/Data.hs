{-# LANGUAGE QuasiQuotes     #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies    #-}
module LogReader.Data where

import           Yesod hiding (loadConfig)
import ClassyPrelude.Yesod hiding (loadConfig)
import Data.Aeson.TH
import qualified Data.Char as C
import LogReader.Watcher (TLogDirMap)

import Control.SuiteSettings

data LogReader = LogReader TLogDirMap

data Settings = Settings
    { jbossPath :: Text
    , applicationPath :: Text
    , tmpPath :: Text
    }

data LogType = Application
             | JBoss
             | Tmp
               deriving (Show, Eq, Read)

data Path = Directory
          | File

-- We have a familiar analogue from mkYesod, with just one extra parameter.
-- We'll discuss that later.
mkYesodSubData "LogReader" [parseRoutes|
/list/#LogType LogFilesR GET -- POST
/view/#LogType/#Text LogSocketR GET
/download/#LogType/#Text LogDownloadR GET
/status WatcherStatusR GET
|]

data LogFiles = LogFiles
    { logFiles :: [Text]
    }
  deriving (Show, Eq)

$(deriveJSON defaultOptions{fieldLabelModifier = drop 0, constructorTagModifier = map C.toLower} ''LogFiles)
$(deriveJSON defaultOptions{fieldLabelModifier = drop 0, constructorTagModifier = map C.toLower} ''Path)

-- readSettings :: IO (Either ParseException LogReaderSettings)
-- readSettings =
--     decodeFileEither "logSettings.yaml"

-- toPath :: Text -> IO Path
-- toPath path =
    

class GetPath a where
    getPath :: Settings -> a -> Text

instance GetPath LogType where
    getPath settings Application = applicationPath settings
    getPath settings JBoss = jbossPath settings
    getPath settings Tmp = tmpPath settings

instance PathPiece LogType where
    fromPathPiece = readMay
    toPathPiece = pack . show

loadSettings :: MonadIO m => FilePath -> m (Either Text Settings)
loadSettings path = do
  config <- loadConfig path
  let get :: (MonadIO m, Configured a) => Text -> ExceptT Text m a
      get key = getVal config $ "logSettings." <> key
                
  runExceptT ( Settings
           <$> get "jbossPath"
           <*> get "applicationPath"
           <*> get "testPath"
             )

withSettings = runSettings . loadSettings