{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Hastory.Cli.Commands.Sync
  ( sync,
    toEntry,
  )
where

import Control.Monad.IO.Unlift (MonadUnliftIO)
import Control.Monad.Reader
import Data.Int
import Data.Maybe
import Database.Persist.Sqlite
import Hastory.API
import Hastory.Cli.Internal
import Hastory.Cli.OptParse.Types
import Hastory.Data
import Hastory.Data.Client.DB hiding (migrateAll)
import Hastory.Data.Server.DB hiding (migrateAll)
import Network.HostName (getHostName)
import System.Exit

-- | Send local entries to sync server and fetch new entries from sync server.
sync :: (MonadReader Settings m, MonadUnliftIO m) => SyncSettings -> m ()
sync (SyncSettings remoteInfo) = do
  HastoryClient {..} <- getHastoryClient remoteInfo
  syncRequest <- getSyncRequest
  let request = createEntryClient hastoryClientToken syncRequest
  response <- liftIO $ runHastoryClient request hastoryClientEnv
  case response of
    Left err -> liftIO $ die ("sync error: " ++ show err)
    Right serverEntries -> mapM_ updateOrInsert serverEntries

getSyncRequest :: (MonadReader Settings m, MonadUnliftIO m) => m SyncRequest
getSyncRequest = do
  maxSyncWitness <- fmap toSqlKey getMaxSyncWitness
  unSyncdLocalEntries <- map entityVal <$> readUnsyncdEntries
  hostname <- liftIO getHostName
  pure $ toSyncRequest unSyncdLocalEntries hostname maxSyncWitness

updateOrInsert :: (MonadReader Settings m, MonadUnliftIO m) => Entity ServerEntry -> m EntryId
updateOrInsert serverEntity = do
  let entry@Entry {..} = toEntry serverEntity
  entity <-
    runDb $
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
  mEntityEntry <- runDb $ selectFirst [EntrySyncWitness !=. Nothing] [Desc EntrySyncWitness]
  let mSyncWitness = entrySyncWitness . entityVal =<< mEntityEntry
  pure $ fromMaybe 0 mSyncWitness

toEntry :: Entity ServerEntry -> Entry
toEntry (Entity serverEntryId ServerEntry {..}) = Entry {..}
  where
    entryText = serverEntryText
    entryUser = serverEntryHostUser
    entryWorkingDir = serverEntryWorkingDir
    entryDateTime = serverEntryDateTime
    entrySyncWitness = Just (fromSqlKey serverEntryId)
    entryHostName = Just serverEntryHostName

getHastoryClient :: (MonadUnliftIO m) => RemoteStorage -> m HastoryClient
getHastoryClient (RemoteStorage baseUrl username password) = do
  eHastoryClient <- liftIO $ mkHastoryClient baseUrl username password
  case eHastoryClient of
    Left _err -> liftIO $ die "Unable to log in to server"
    Right client -> pure client

readUnsyncdEntries :: (MonadReader Settings m, MonadUnliftIO m) => m [Entity Entry]
readUnsyncdEntries = runDb $ selectList [EntrySyncWitness ==. Nothing] []
