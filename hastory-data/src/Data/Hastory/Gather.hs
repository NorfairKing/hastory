{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.Hastory.Gather
    ( gatherEntryWith
    ) where

import Import

import Data.Text (Text)
import qualified Data.Time.LocalTime as Time
import Network.HostName (getHostName)
import System.Posix.User (getEffectiveUserName)

import Data.Hastory.Types

gatherEntryWith :: Text -> IO Entry
gatherEntryWith text = do
    curtime <- Time.getZonedTime
    curdir <- getCurrentDir
    hostname <- getHostName
    user <- getEffectiveUserName
    pure
        Entry
            { entryText = text
            , entryDateTime = curtime
            , entryWorkingDir = curdir
            , entryHostName = hostname
            , entryUser = user
            }
