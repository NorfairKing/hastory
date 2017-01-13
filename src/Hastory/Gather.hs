{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Hastory.Gather where

import Introduction

import qualified Data.Aeson as JSON
import qualified Data.ByteString.Lazy as LB
import qualified Data.Text.IO as T
import qualified Data.Time.LocalTime as Time

import Hastory.Types
import Hastory.Internal

gather :: IO ()
gather = do
    curtime <- Time.getZonedTime
    curdir <- getCurrentDir
    text <- T.getContents
    storeHistory
        Entry
        {entryText = text, entryDateTime = curtime, entryWorkingDir = curdir}

storeHistory :: Entry -> IO ()
storeHistory entry = do
    hFile <- histfile
    ensureDir $ parent hFile
    LB.appendFile (toFilePath hFile) $ JSON.encode entry <> "\n"
