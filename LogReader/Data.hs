{-# LANGUAGE QuasiQuotes     #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies    #-}
module LogReader.Data where

import           Yesod
import ClassyPrelude.Yesod hiding (FilePath)
import Data.Map as M

data LogReader = LogReader

data Directory = Directory Text
data FilePath = FilePath Text
data FileName = FileName Text

data LogType = Application
             | JBoss
             | Tmp
               deriving (Show, Eq, Read)

type FileMap = M.Map FileName FilePath
type DirMap = M.Map Directory FileMap

-- We have a familiar analogue from mkYesod, with just one extra parameter.
-- We'll discuss that later.
mkYesodSubData "LogReader" [parseRoutes|
/#LogType LogReaderR GET -- POST
|]

class GetPath a where
    getPath :: a -> Directory

instance GetPath LogType where
    getPath Application = Directory "/opt/appian/logs/"
    getPath JBoss = Directory "/opt/jboss-eap-6.4/standalone/log/"
    getPath Tmp = Directory "/private/tmp/"

instance PathPiece LogType where
    fromPathPiece = readMay
    toPathPiece = pack . show