{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Hastory.Internal where

import Introduction

import qualified Data.Aeson as JSON
import qualified Data.ByteString as SB
import qualified Data.ByteString.Lazy as LB
import qualified Data.ByteString.Lazy.Char8 as LB8
import Data.HashMap.Lazy (HashMap)
import qualified Data.HashMap.Lazy as HM
import Data.Hashable (Hashable)
import Data.List (unwords)
import qualified Data.Text.IO as T
import qualified Data.Time.Clock as Time
import qualified Data.Time.LocalTime as Time

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
