{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}

module Hastory.Cli.Commands.Gather where

import Control.Monad.Catch
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Control.Monad.Reader
import Data.Text (Text)
import qualified Data.Text.IO as T
import Database.Persist.Sqlite (Entity, upsertBy)

import Data.Hastory
import Hastory.Cli.Internal
import Hastory.Cli.OptParse.Types

gather :: (MonadReader Settings m, MonadThrow m, MonadUnliftIO m) => m (Entity Entry)
gather = do
  text <- liftIO T.getContents
  gatherFrom text

gatherFrom :: (MonadReader Settings m, MonadThrow m, MonadUnliftIO m) => Text -> m (Entity Entry)
gatherFrom text = do
  entry <- liftIO $ gatherEntryWith text
  storeHistory entry

storeHistory :: (MonadReader Settings m, MonadThrow m, MonadUnliftIO m) => Entry -> m (Entity Entry)
storeHistory entry@Entry {..} = runDb (upsertBy uniqueRecord entry noUpdate)
  where
    uniqueRecord = EntryData entryText entryWorkingDir entryDateTime entryUser
    noUpdate = []
