{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Hastory.Internal where

import Import

import qualified Data.Aeson as JSON
import qualified Data.ByteString.Lazy.Char8 as LB8

import Hastory.Types

hastoryDir :: IO (Path Abs Dir)
hastoryDir = do
    home <- getHomeDir
    pure $ home </> $(mkRelDir ".hastory")


histfile :: IO (Path Abs File)
histfile =
    fmap (</> $(mkRelFile "commandhistory.log")) hastoryDir

getHistory :: IO [Entry]
getHistory = do
    hFile <- histfile
    contents <- readFile hFile
    let encodedEntries = LB8.lines contents
    let entries = catMaybes $ map JSON.decode encodedEntries
    pure entries
