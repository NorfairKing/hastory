{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Hastory.Cli.Commands.Sync
  ( sync
  , toEntry
  ) where

import Control.Monad.Catch
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Control.Monad.Reader
import Data.Int
import Database.Persist.Sqlite
import Network.HostName (getHostName)
import System.Exit

import Data.Hastory
import Data.Hastory.API
import Hastory.Cli.Internal
import Hastory.Cli.OptParse.Types

-- | Send local entries to sync server and fetch new entries from sync server.
sync :: (MonadThrow m, MonadReader Settings m, MonadUnliftIO m) => RemoteStorageClientInfo -> m ()
sync remoteInfo = do
  HastoryClient {..} <- getHastoryClient remoteInfo
  maxSyncWitness <- fmap toSqlKey getMaxSyncWitness
  syncRequest <- getSyncRequest
  let request = createEntryClient hastoryClientToken syncRequest maxSyncWitness
  response <- liftIO $ runHastoryClient request hastoryClientEnv
  case response of
    Left err -> liftIO $ die ("sync error: " ++ show err)
    Right serverEntries -> mapM_ updateOrInsert serverEntries

getSyncRequest :: (MonadThrow m, MonadReader Settings m, MonadUnliftIO m) => m SyncRequest
getSyncRequest = do
  unSyncdLocalEntries <- map entityVal <$> readUnsyncdEntries
  hostname <- liftIO getHostName
  pure $ toSyncRequest unSyncdLocalEntries hostname

updateOrInsert ::
     (MonadThrow m, MonadReader Settings m, MonadUnliftIO m) => Entity ServerEntry -> m EntryId
updateOrInsert serverEntity = do
  let entry = toEntry serverEntity
  mEntity <-
    runDb $
    selectFirst
      [ EntryText ==. entryText entry
      , EntryWorkingDir ==. entryWorkingDir entry
      , EntryDateTime ==. entryDateTime entry
      , EntryUser ==. entryUser entry
      , EntrySyncWitness ==. Nothing
      ]
      []
  case mEntity of
    Nothing -> runDb (insert entry)
    Just localEntity -> do
      let localKey = entityKey localEntity
      runDb $ replace localKey entry
      pure localKey

-- | Mechanically, the syncWitness is the id (Int64) of the entry on the remote
-- server. We assume that the client knows about all entries up to and including
-- the maximum syncWitness in the local database. By sending the maximum
-- syncWitness to the remote server when fetching entries, the server will only
-- return entries that are unknown to the client.
getMaxSyncWitness :: (MonadThrow m, MonadReader Settings m, MonadUnliftIO m) => m Int64
getMaxSyncWitness = do
  mEntityEntry <- runDb (selectFirst [EntrySyncWitness !=. Nothing] [Desc EntrySyncWitness])
  case mEntityEntry of
    Nothing -> pure 0
    Just entity ->
      case entrySyncWitness (entityVal entity) of
        Nothing -> pure 0
        Just syncWitness -> pure syncWitness

toEntry :: Entity ServerEntry -> Entry
toEntry entity = Entry {..}
  where
    entryText = serverEntryText (entityVal entity)
    entryUser = serverEntryHostUser (entityVal entity)
    entryWorkingDir = serverEntryWorkingDir (entityVal entity)
    entryDateTime = serverEntryDateTime (entityVal entity)
    entrySyncWitness = Just . fromSqlKey . entityKey $ entity

getHastoryClient :: (MonadThrow m, MonadUnliftIO m) => RemoteStorageClientInfo -> m HastoryClient
getHastoryClient (RemoteStorageClientInfo baseUrl username password) = do
  eHastoryClient <- liftIO $ mkHastoryClient baseUrl username password
  case eHastoryClient of
    Left _err -> liftIO $ die "Unable to log in to server"
    Right client -> pure client

readUnsyncdEntries :: (MonadThrow m, MonadReader Settings m, MonadUnliftIO m) => m [Entity Entry]
readUnsyncdEntries = runDb $ selectList [EntrySyncWitness ==. Nothing] []
