{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}

module Hastory.Cli.Commands.Gather where

import Control.Monad.IO.Unlift (MonadUnliftIO)
import Control.Monad.Reader
import Data.Text (Text)
import qualified Data.Text.IO as T
import Database.Persist.Sqlite (Entity, upsertBy)
import Hastory.API.Gather
import Hastory.Cli.Internal
import Hastory.Cli.OptParse.Types
import Hastory.Data.Client.DB

gather :: (MonadReader Settings m, MonadUnliftIO m) => m (Entity Entry)
gather = do
  text <- liftIO T.getContents
  gatherFrom text

gatherFrom :: (MonadReader Settings m, MonadUnliftIO m) => Text -> m (Entity Entry)
gatherFrom text = do
  entry <- liftIO $ gatherEntryWith text
  storeHistory entry

storeHistory :: (MonadReader Settings m, MonadUnliftIO m) => Entry -> m (Entity Entry)
storeHistory entry@Entry {..} = runDb (upsertBy uniqueRecord entry noUpdate)
  where
    uniqueRecord = EntryData entryText entryWorkingDir entryDateTime entryUser
    noUpdate = []
