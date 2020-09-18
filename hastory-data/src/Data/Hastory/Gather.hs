{-# LANGUAGE FlexibleContexts #-}

module Data.Hastory.Gather
  ( gatherEntryWith
  ) where

import Data.Hastory.Types

import Data.Text (Text)
import qualified Data.Text as T
import Path.IO (getCurrentDir)
import System.Posix.User (getEffectiveUserName)

import qualified Data.Time as Time

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
      }
