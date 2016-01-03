{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TemplateHaskell       #-}
module BulkDownloader.Downloader
    ( module BulkDownloader.Data
    , module BulkDownloader.Downloader
    ) where
import           BulkDownloader.Data
import           Yesod
import           ClassyPrelude.Yesod hiding ((<>))
