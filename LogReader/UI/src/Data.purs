module LogReader.Data
    ( module Data.JSON
    , LogFiles(..)
    )
    where

import Prelude
import Data.JSON
import Data.Array
import LogReader.Bootstrap
import Data.Monoid

data LogFiles = LogFiles
    { logFiles :: Array String
    }

instance logFilesShow :: Show LogFiles where
    show (LogFiles {logFiles = lf}) =
        "LogFiles " ++ show lf

instance logFilesFromJSON :: FromJSON LogFiles where
    parseJSON (JObject o) = do
      logFiles <- o .: "logFiles"
      return $ LogFiles {logFiles: logFiles}
    parseJSON _ = fail "Could not parse LogFiles"

instance logFilesMonoid :: Monoid LogFiles where
    mempty = LogFiles {logFiles: mempty}

instance logFilesSemigroup :: Semigroup LogFiles where
    append (LogFiles {logFiles = a}) (LogFiles {logFiles = b}) =
        LogFiles {logFiles: a <> b}