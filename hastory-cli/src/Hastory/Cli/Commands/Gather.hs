{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module Hastory.Cli.Commands.Gather where

import Import

import Data.Hastory
import Data.Hastory.API
import Hastory.Cli.Internal
import Hastory.Cli.OptParse.Types

import Data.Text (Text)

import qualified Data.Text.IO as T
import qualified Database.Persist.Sqlite as SQL

gather :: (MonadReader Settings m, MonadThrow m, MonadUnliftIO m) => m ()
gather = do
    text <- liftIO T.getContents
    gatherFrom text

gatherFrom :: (MonadReader Settings m, MonadThrow m, MonadUnliftIO m) => Text -> m ()
gatherFrom text = do
    entry <- liftIO $ gatherEntryWith text
    storeHistory entry

sendEntryToStorageServer :: (MonadIO m, MonadThrow m, MonadReader Settings m) => Entry -> m ()
sendEntryToStorageServer entry =
  asks remoteStorageClient >>= \case
    Nothing -> pure ()
    Just client ->
      liftIO $ print =<< runHastoryClientM client (\t -> appendCommand (Just t) entry)

-- | TODO: Make sendEntryToStorageServer async
storeHistory ::
       (MonadReader Settings m, MonadThrow m, MonadUnliftIO m) => Entry -> m ()
storeHistory entry = do
  sendEntryToStorageServer entry
  runDb (SQL.insert_ entry)
