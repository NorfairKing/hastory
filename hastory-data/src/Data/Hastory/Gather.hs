{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.Hastory.Gather
    ( gatherEntryWith
    ) where

import Import

import Data.Hastory.Types

import Data.Text (Text, pack)
import Network.HostName (getHostName)
import System.Posix.User (getEffectiveUserName)

import qualified Data.Time as Time

gatherEntryWith :: Text -> IO Entry
gatherEntryWith text = do
    curtime <- Time.getCurrentTime
    curdir <- getCurrentDir
    hostname <- getHostName
    user <- getEffectiveUserName
    pure
        Entry
            { entryText = text
            , entryDateTime = curtime
            , entryWorkingDir = curdir
            , entryHostName = (pack hostname)
            , entryUser = (pack user)
            }
