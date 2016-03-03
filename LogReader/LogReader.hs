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
import Data.Yaml hiding (encode)

withSettings f = do
  eSettings <- liftIO $ readSettings
  case eSettings of
    Left exception ->
        error $ prettyPrintParseException exception
    Right settings ->
        f settings

getDirectoryContents :: (GetPath dir, MonadHandler site) => dir -> LogReaderSettings -> site [Text]
getDirectoryContents dir settings = do
  contents <- liftIO $ SysDir.getDirectoryContents $ unpack $ getPath settings dir
  return $ map pack $ filter (\x -> not $ x `elem` [".", ".."]) contents

getLogFilesR :: Yesod master => LogType -> HandlerT LogReader (HandlerT master IO) ()
getLogFilesR logType =
  withSettings $ \settings -> do
    list <- LogFiles <$> getDirectoryContents logType settings
    webSockets $
       sendTextData $ encode list

getLogSocketR :: Yesod master => LogType -> Text -> HandlerT LogReader (HandlerT master IO) ()
getLogSocketR logType logName =
  withSettings $ \settings -> do
    let logKey = toLogKey' (getPath settings logType) logName
    (LogReader tLogDirMap) <- getYesod
    mChans <- liftIO $ tailFile tLogDirMap logKey
    webSockets $ mainLoop mChans logKey
  -- eSettings <- liftIO $ readSettings
  -- case eSettings of
  --   Left exception ->
  --       error $ prettyPrintParseException exception
  --   Right settings -> do
  --       let logKey = toLogKey' (getPath settings logType) logName
  --       (LogReader tLogDirMap) <- getYesod
  --       mChans <- liftIO $ tailFile tLogDirMap logKey
  --       webSockets $ mainLoop mChans logKey

mainLoop :: MonadIO m
         => (Maybe LogChannel, Maybe DirChannel)
         -> LogKey
         -> WebSocketsT m ()
mainLoop (Nothing, Nothing) _ = fail "No channels?"
mainLoop (Nothing, _) _ = fail "No read channel?"
mainLoop (_, Nothing) _ = fail "No write channel?"
mainLoop (Just rChan, Just wChan) logKey =
    let check :: MonadIO m => Either t1 t2 -> WebSocketsT m ()
        check (Left _) = liftIO $ atomically $ writeDChan wChan $ Closed logKey
        check (Right _) = mainLoop (Just rChan, Just wChan) logKey
    in do
      msg <- liftIO $ atomically $ readFChan rChan
      sendTextDataE (encode msg) >>= check
      -- case msg of
      --   Data content -> sendTextDataE content >>= check
      --   Ping -> sendTextDataE ("Ping" :: Text) >>= check
      --   Closed _ -> return ()
   -- f = race_
   --       (forever $ atomically $ do
   --         msg <- readFChan rChan
   --         case msg of
   --           Data content -> sendTextData content
   --           _ -> sendTextData "Ping"
   --       )
   --       pingSocket wChan logKey

-- pingSocket wChan logKey = do
--         threadDelay delay
--         msg <- readFChan
--         case msg of
--           Left _ -> return ()
--           Right _ -> pingSocket wChan logKey

-- mainLoop (Just rChan, Just wChan) logKey =
--     let check :: MonadIO m => Either t1 t2 -> WebSocketsT m ()
--         check (Left _) = liftIO $ writeDChan wChan $ Closed logKey
--         check (Right _) = mainLoop (Just rChan, Just wChan) logKey

--     in do
--       liftIO $ putStrLn "Waiting for content"
--       result <- liftIO $ readFChan rChan
--       liftIO $ putStrLn $ "Received " <> tshow result
--       case result of
--         Ping -> sendTextDataE ("Ping" :: Text) >>= check
--         Data content -> sendTextDataE content >>= check
--         Closed _ -> sendTextData ("Close" :: Text)

instance (Yesod master) => YesodSubDispatch LogReader (HandlerT master IO) where
    yesodSubDispatch = $(mkYesodSubDispatch resourcesLogReader)