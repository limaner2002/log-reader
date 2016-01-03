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
-- import Yesod.Table (Table)
-- import qualified Yesod.Table as Table
import Lucid (HtmlT)
import qualified Lucid as L
import Lucid.Html5
import LucidBootstrapLayout.Bootstrap
import LucidBootstrapLayout.Table
import Yesod.Lucid
import Data.Monoid
import Debug.Trace
-- import Text.Blaze.Internal (MarkupM)
-- import LucidBootstrapLayout.Table
-- import Blaze.Html

-- type LucidHtml = HtmlT MarkupM ()

getDirectoryContents :: (GetPath dir, MonadHandler site) => dir -> site [Text]
getDirectoryContents dir = do
  let (Directory path) = getPath dir
  contents <- liftIO $ SysDir.getDirectoryContents $ unpack path
  return $ map pack $ filter (\x -> not $ x `elem` [".", ".."]) contents

getLogReaderR :: (Yesod master, Bootstrap master) => LogType -> HandlerT LogReader (HandlerT master IO) LucidHtml
getLogReaderR logType = do
  cts <- getDirectoryContents logType
  toMaster <- getRouteToParent
  let tableWidget = buildBootstrap fileTable rows
      rows = map (\fName -> Row $ fileLink fName (trace (show link) (concat $ fst link))) cts
      link = renderRoute $ toMaster $ LogReaderR logType
  lift $ mainLayout tableWidget

data Row m =
    Row
    { fileRef :: FileLink m}

data FileLink m = FileLink (HtmlT m ())

fileLink :: Monad m => Text -> Text -> FileLink m
fileLink fName url = FileLink $ a_ [href_ url] (L.toHtml fName)

fileTable :: Monad m => Table (HtmlT m ()) (Row m)
fileTable = mempty
    <> column "File Name" (unwrapLink . fileRef)

unwrapLink :: Monad m => FileLink m -> (HtmlT m ())
unwrapLink (FileLink v) = v

instance (Yesod master, Bootstrap master) => YesodSubDispatch LogReader (HandlerT master IO) where
    yesodSubDispatch = $(mkYesodSubDispatch resourcesLogReader)