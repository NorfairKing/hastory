{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Hastory.Gather where

import Import

import qualified Data.Aeson as JSON
import qualified Data.ByteString.Lazy as LB
import qualified Data.Text.IO as T
import qualified Data.Time.LocalTime as Time
import Network.HostName (getHostName)
import System.Posix.User (getEffectiveUserName)

import Hastory.Internal
import Hastory.Types

gather :: IO ()
gather = do
    curtime <- Time.getZonedTime
    curdir <- getCurrentDir
    text <- T.getContents
    hostname <- getHostName
    user <- getEffectiveUserName
    storeHistory
        Entry
        { entryText = text
        , entryDateTime = curtime
        , entryWorkingDir = curdir
        , entryHostName = hostname
        , entryUser = user
        }

storeHistory :: Entry -> IO ()
storeHistory entry = do
    hFile <- histfile
    ensureDir $ parent hFile
    LB.appendFile (toFilePath hFile) $ JSON.encode entry <> "\n"
