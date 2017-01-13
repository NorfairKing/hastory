{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Hastory.Internal where

import Introduction

import qualified Data.Aeson as JSON
import qualified Data.ByteString.Lazy.Char8 as LB8

import Hastory.Types

histfile :: IO (Path Abs File)
histfile = do
    home <- getHomeDir
    pure $ home </> $(mkRelDir ".hastory") </> $(mkRelFile "commandhistory.log")

getHistory :: IO [Entry]
getHistory = do
    hFile <- histfile
    contents <- readFile hFile
    let encodedEntries = LB8.lines contents
    let entries = catMaybes $ map JSON.decode encodedEntries
    pure entries
