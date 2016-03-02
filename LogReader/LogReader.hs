{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TemplateHaskell       #-}

module LogReader.LogReader
    ( module LogReader.Data
    , module LogReader.LogReader
    , module LogReader.Watcher
    ) where

import           LogReader.Data
import           Yesod
import           ClassyPrelude.Yesod -- hiding (writeChan, readChan)
import qualified System.Directory as SysDir
import Data.Aeson (encode)
import Yesod.WebSockets
import LogReader.Watcher

getDirectoryContents :: (GetPath dir, MonadHandler site) => dir -> site [Text]
getDirectoryContents dir = do
  contents <- liftIO $ SysDir.getDirectoryContents $ unpack $ getPath dir
  return $ map pack $ filter (\x -> not $ x `elem` [".", ".."]) contents

getLogFilesR :: Yesod master => LogType -> HandlerT LogReader (HandlerT master IO) ()
getLogFilesR logType = do
  list <- LogFiles <$> getDirectoryContents logType
  webSockets $
    sendTextData $ encode list

getLogSocketR :: Yesod master => LogType -> Text -> HandlerT LogReader (HandlerT master IO) ()
getLogSocketR logType logName = do
  let logKey = toLogKey' (getPath logType) logName
  (LogReader tLogDirMap) <- getYesod
  mChans <- liftIO $ tailFile tLogDirMap logKey
  webSockets $ mainLoop mChans logKey

mainLoop :: MonadIO m
         => (Maybe LogChannel, Maybe DirChannel)
         -> LogKey
         -> WebSocketsT m ()
mainLoop (Nothing, Nothing) _ = fail "No channels?"
mainLoop (Nothing, _) _ = fail "No read channel?"
mainLoop (_, Nothing) _ = fail "No write channel?"
mainLoop (Just rChan, Just wChan) logKey =
    let check :: MonadIO m => Either t1 t2 -> WebSocketsT m ()
        check (Left _) = liftIO $ writeDChan wChan $ Closed logKey
        check (Right _) = mainLoop (Just rChan, Just wChan) logKey

    in do
      liftIO $ putStrLn "Waiting for content"
      result <- liftIO $ readFChan rChan
      liftIO $ putStrLn $ "Received " <> tshow result
      case result of
        Ping -> sendTextDataE ("Ping" :: Text) >>= check
        Data content -> sendTextDataE content >>= check
        Closed _ -> sendTextData ("Close" :: Text)

instance (Yesod master) => YesodSubDispatch LogReader (HandlerT master IO) where
    yesodSubDispatch = $(mkYesodSubDispatch resourcesLogReader)