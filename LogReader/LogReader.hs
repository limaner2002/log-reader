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
import           ClassyPrelude.Yesod hiding ((</>))
import qualified System.Directory as SysDir
import Data.Aeson (encode)
import Yesod.WebSockets
import LogReader.Watcher
import Data.Yaml hiding (encode)
import qualified Data.Text as T
import System.IO (hFlush, stdout)
import Control.Exception.Lifted
import qualified Data.Conduit.Combinators as CC
import Data.Conduit
import Yesod.Default.Util (widgetFileNoReload)
import Text.Julius (rawJS)

-- withSettings f = do
--   eSettings <- liftIO $ readSettings
--   case eSettings of
--     Left exception ->
--         error $ prettyPrintParseException exception
--     Right (LogReaderSettings jboss application tmp) ->
-- #ifdef WINDOWS
--         f $ LogReaderSettings (rep jboss) (rep application) (rep tmp)
--       where
-- 	rep = T.replace "/" "\\"
-- #else
--         f $ LogReaderSettings jboss application tmp
-- #endif

getDirectoryContents :: (GetPath dir, MonadHandler site) => dir -> Settings -> site [Text]
getDirectoryContents dir settings = do
  contents <- liftIO $ SysDir.getDirectoryContents $ fromAbsDir $ getPath settings dir
  return $ map pack $ filter (\x -> not $ x `elem` [".", ".."]) contents

getLogFilesR :: Yesod master => LogType -> HandlerT LogReader (HandlerT master IO) ()
getLogFilesR logType =
    withSettings "settings.conf" $ \settings -> do
      let path = pack $ fromAbsDir $ getPath settings logType :: Text
      webSockets $ 
          CC.sourceDirectoryDeep False (unpack path)
#ifdef WINDOWS
             $$ CC.map (encode . stripPrefix (path <> "\\") . pack)
#else
             $$ CC.map (encode . stripPrefix path . pack)
#endif
             =$ sinkWSText
    

getLogSocketR :: Yesod master => LogType -> Path Rel File -> HandlerT LogReader (HandlerT master IO) Html
getLogSocketR logType logName =
  withSettings "settings.conf" $ \settings -> do
    let logKey = toLogKey' $ (getPath settings logType) </> logName

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
    lift $ defaultLayout $(widgetFileNoReload def "logFile")

getLogDownloadR :: Yesod master => LogType -> (Path Rel File) -> HandlerT LogReader (HandlerT master IO) TypedContent
getLogDownloadR logType logName = withSettings "settings.conf" $ \settings ->
    sendFile typePlain $ fromLogKey (logKey settings)
  where
    logKey settings = toLogKey' $ (getPath settings logType) </> logName

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