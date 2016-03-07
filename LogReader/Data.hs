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

data Directory = Directory Text
data FilePath = FilePath Text
data FileName = FileName Text

data LogType = Application
             | JBoss
             | Tmp
               deriving (Show, Eq, Read)

type FileMap = Map FileName FilePath
type DirMap = Map Directory FileMap

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

readSettings :: IO (Either ParseException LogReaderSettings)
readSettings =
    decodeFileEither "logSettings.yaml"

class GetPath a where
    getPath :: LogReaderSettings -> a -> Text

instance GetPath LogType where
    getPath settings Application = applicationPath settings
    getPath settings JBoss = jbossPath settings
    getPath settings Tmp = tmpPath settings

instance PathPiece LogType where
    fromPathPiece = readMay
    toPathPiece = pack . show