{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}

module Hastory.Internal where

import Import

import qualified Data.Aeson as JSON
import qualified Data.ByteString.Lazy as LB
import qualified Data.ByteString.Lazy.Char8 as LB8

import Hastory.OptParse.Types
import Hastory.Types

hastoryDir :: MonadReader Settings m => m (Path Abs Dir)
hastoryDir = asks setCacheDir

histfile :: MonadReader Settings m => m (Path Abs File)
histfile = fmap (</> $(mkRelFile "commandhistory.log")) hastoryDir

getHistory :: (MonadIO m, MonadReader Settings m) => m [Entry]
getHistory = do
    hFile <- histfile
    contents <- liftIO $ LB.readFile $ toFilePath hFile
    let encodedEntries = LB8.lines contents
    let entries = catMaybes $ map JSON.decode encodedEntries
    pure entries
