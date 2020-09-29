{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Hastory.Cli.Commands.Sync
  ( sync
  , toEntry
  ) where

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
sync :: (MonadReader Settings m, MonadUnliftIO m) => RemoteStorageClientInfo -> m ()
sync remoteInfo = do
  HastoryClient {..} <- getHastoryClient remoteInfo
  maxSyncWitness <- fmap toSqlKey getMaxSyncWitness
  syncRequest <- getSyncRequest
  let request = createEntryClient hastoryClientToken syncRequest maxSyncWitness
  response <- liftIO $ runHastoryClient request hastoryClientEnv
  case response of
    Left err -> liftIO $ die ("sync error: " ++ show err)
    Right serverEntries -> mapM_ updateOrInsert serverEntries

getSyncRequest :: (MonadReader Settings m, MonadUnliftIO m) => m SyncRequest
getSyncRequest = do
  unSyncdLocalEntries <- map entityVal <$> readUnsyncdEntries
  hostname <- liftIO getHostName
  pure $ toSyncRequest unSyncdLocalEntries hostname

updateOrInsert :: (MonadReader Settings m, MonadUnliftIO m) => Entity ServerEntry -> m EntryId
updateOrInsert serverEntity = do
  let entry@Entry {..} = toEntry serverEntity
  entity <-
    runDb' $
    upsertBy
      (EntryData entryText entryWorkingDir entryDateTime entryUser)
      entry
      [EntrySyncWitness =. entrySyncWitness]
  pure $ entityKey entity

-- | Mechanically, the syncWitness is the id (Int64) of the entry on the remote
-- server. We assume that the client knows about all entries up to and including
-- the maximum syncWitness in the local database. By sending the maximum
-- syncWitness to the remote server when fetching entries, the server will only
-- return entries that are unknown to the client.
getMaxSyncWitness :: (MonadReader Settings m, MonadUnliftIO m) => m Int64
getMaxSyncWitness = do
  mEntityEntry <- runDb' $ selectFirst [EntrySyncWitness !=. Nothing] [Desc EntrySyncWitness]
  let mSyncWitness = entrySyncWitness . entityVal =<< mEntityEntry
  case mSyncWitness of
    Nothing -> pure 0
    Just i -> pure i

toEntry :: Entity ServerEntry -> Entry
toEntry (Entity serverEntryId ServerEntry {..}) = Entry {..}
  where
    entryText = serverEntryText
    entryUser = serverEntryHostUser
    entryWorkingDir = serverEntryWorkingDir
    entryDateTime = serverEntryDateTime
    entrySyncWitness = Just (fromSqlKey serverEntryId)

getHastoryClient :: (MonadUnliftIO m) => RemoteStorageClientInfo -> m HastoryClient
getHastoryClient (RemoteStorageClientInfo baseUrl username password) = do
  eHastoryClient <- liftIO $ mkHastoryClient baseUrl username password
  case eHastoryClient of
    Left _err -> liftIO $ die "Unable to log in to server"
    Right client -> pure client

readUnsyncdEntries :: (MonadReader Settings m, MonadUnliftIO m) => m [Entity Entry]
readUnsyncdEntries = runDb' $ selectList [EntrySyncWitness ==. Nothing] []
