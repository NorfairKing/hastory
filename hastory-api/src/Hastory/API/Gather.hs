{-# LANGUAGE FlexibleContexts #-}

module Hastory.API.Gather
  ( gatherEntryWith
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Time as Time
import Path.IO (getCurrentDir)
import System.Posix.User (getEffectiveUserName)

import Hastory.Data.Client.DB (Entry(..))

gatherEntryWith :: Text -> IO Entry
gatherEntryWith text = do
  curtime <- Time.getCurrentTime
  curdir <- getCurrentDir
  user <- getEffectiveUserName
  pure
    Entry
      { entryText = text
      , entryDateTime = curtime
      , entryWorkingDir = curdir
      , entryUser = T.pack user
      , entrySyncWitness = Nothing
      , entryHostName = Nothing
      }
