{-# LANGUAGE QuasiQuotes     #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies    #-}
module LogReader.Data where

import           Yesod
import ClassyPrelude.Yesod hiding (FilePath)
import Data.Yaml
import Data.Aeson.TH
import qualified Data.Char as C
import LogReader.Watcher (TLogDirMap)

data LogReader = LogReader TLogDirMap

data LogReaderSettings = LogReaderSettings
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
$(deriveJSON defaultOptions{fieldLabelModifier = drop 0, constructorTagModifier = map C.toLower} ''LogReaderSettings)
$(deriveJSON defaultOptions{fieldLabelModifier = drop 0, constructorTagModifier = map C.toLower} ''Path)

readSettings :: IO (Either ParseException LogReaderSettings)
readSettings =
    decodeFileEither "logSettings.yaml"

-- toPath :: Text -> IO Path
-- toPath path =
    

class GetPath a where
    getPath :: LogReaderSettings -> a -> Text

instance GetPath LogType where
    getPath settings Application = applicationPath settings
    getPath settings JBoss = jbossPath settings
    getPath settings Tmp = tmpPath settings

instance PathPiece LogType where
    fromPathPiece = readMay
    toPathPiece = pack . show