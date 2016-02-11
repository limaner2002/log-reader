{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TemplateHaskell       #-}
module LogReader.LogReader
    ( module LogReader.Data
    , module LogReader.LogReader
    ) where

import           LogReader.Data
import           Yesod
import           ClassyPrelude.Yesod hiding ((<>))
import qualified System.Directory as SysDir
import Data.Aeson (encode)
import Yesod.WebSockets

getDirectoryContents :: (GetPath dir, MonadHandler site) => dir -> site [Text]
getDirectoryContents dir = do
  let (Directory path) = getPath dir
  contents <- liftIO $ SysDir.getDirectoryContents $ unpack path
  return $ map pack $ filter (\x -> not $ x `elem` [".", ".."]) contents

getLogReaderR :: Yesod master => LogType -> HandlerT LogReader (HandlerT master IO) ()
getLogReaderR logType = do
  list <- LogFiles <$> getDirectoryContents logType
  webSockets $
    sendTextData $ encode list

--  return $ toJSON list

instance (Yesod master) => YesodSubDispatch LogReader (HandlerT master IO) where
    yesodSubDispatch = $(mkYesodSubDispatch resourcesLogReader)