{-# LANGUAGE QuasiQuotes     #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies    #-}
module LogReader.Data where

import           Yesod
import ClassyPrelude.Yesod hiding (FilePath)
import Data.Aeson.TH
import qualified Data.Char as C
import LogReader.Watcher (TLogDirMap)

data LogReader = LogReader TLogDirMap

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
/#LogType LogFilesR GET -- POST
/#LogType/#Text LogSocketR GET
|]

data LogFiles = LogFiles
    { logFiles :: [Text]
    }
  deriving (Show, Eq)

$(deriveJSON defaultOptions{fieldLabelModifier = drop 0, constructorTagModifier = map C.toLower} ''LogFiles)

class GetPath a where
    getPath :: a -> Text

instance GetPath LogType where
    getPath Application = "/opt/appian/logs/"
    getPath JBoss = "/opt/jboss-eap-6.4/standalone/log/"
    getPath Tmp = "/private/tmp/"

instance PathPiece LogType where
    fromPathPiece = readMay
    toPathPiece = pack . show