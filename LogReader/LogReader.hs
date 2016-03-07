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
import qualified Data.Text as T
import System.IO (hFlush, stdout)
import Control.Exception.Lifted

withSettings f = do
  eSettings <- liftIO $ readSettings
  case eSettings of
    Left exception ->
        error $ prettyPrintParseException exception
    Right (LogReaderSettings jboss application tmp) ->
#ifdef WINDOWS
        f $ LogReaderSettings (rep jboss) (rep application) (rep tmp)
      where
	rep = T.replace "/" "\\"
#else
        f $ LogReaderSettings jboss application tmp
#endif

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

    -- checkExists logKey $ do
    (LogReader tLogDirMap) <- getYesod
    webSockets $ bracket
                 (liftIO $ checkExists logKey $ tailFile tLogDirMap logKey)
                 (\(_, Just wChan) -> liftIO $ do
                    putStrLn $ "Sending Close " <> tshow logKey
                    hFlush stdout
                    atomically $ writeDChan wChan $ Closed logKey
                 )
                 (\(logChan, dirChan) ->
                    race_ (sendLoop $ fileUpdates logChan) (sendLoop $ checkConnection dirChan)
                 )

getLogDownloadR :: Yesod master => LogType -> Text -> HandlerT LogReader (HandlerT master IO) TypedContent
getLogDownloadR logType logName = withSettings $ \settings ->
    sendFile typePlain $ fromLogKey (logKey settings)
  where
    logKey settings = toLogKey' (getPath settings logType) logName

checkExists logKey f = do
    exists <- SysDir.doesFileExist (fromLogKey logKey)
    case exists of
      True -> f
      False -> error "File does not exist"

getWatcherStatusR :: Yesod master => HandlerT LogReader (HandlerT master IO) Html
getWatcherStatusR = do
  (LogReader tLogDirMap) <- getYesod
  status <- liftIO $ showLogMap tLogDirMap
  lift $ defaultLayout $ do
         [whamlet|
                   <h2>LogDirMap
                       #{status}
         |]

sendLoop :: MonadIO m
         => WebSocketsT m (Either a b)
         -> WebSocketsT m ()
sendLoop f = do
  result <- f
  case result of
    Left _ -> return ()
    Right _ -> sendLoop f

fileUpdates :: MonadIO m
            => Maybe LogChannel
            -> WebSocketsT m (Either SomeException ())
fileUpdates Nothing = error "No log channel?"
fileUpdates (Just chan) = do
  msg <- liftIO $ atomically $ readFChan chan
  sendTextDataE $ encode msg

checkConnection :: MonadIO m
                => Maybe DirChannel
                -> WebSocketsT m (Either SomeException ())
checkConnection Nothing = error "No dir channel?"
checkConnection (Just chan) = do
  msg <- liftIO $ atomically $ getPing chan
  sendTextDataE $ encode msg

instance (Yesod master) => YesodSubDispatch LogReader (HandlerT master IO) where
    yesodSubDispatch = $(mkYesodSubDispatch resourcesLogReader)