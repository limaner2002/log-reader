module LogReader.Data
    ( module Data.JSON
    , LogFiles(..)
    )
    where

import Prelude
import Data.JSON
import Data.Array
import LogReader.Bootstrap

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

