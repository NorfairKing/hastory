{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Hastory.Cli.Commands.Sync
  ( sync
  ) where

import Control.Monad.Catch
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Control.Monad.Reader
import Database.Persist.Sqlite
import Network.HostName (getHostName)
import System.Exit

import Data.Hastory
import Data.Hastory.API
import Hastory.Cli.Internal
import Hastory.Cli.OptParse.Types

sync :: (MonadThrow m, MonadReader Settings m, MonadUnliftIO m) => RemoteStorageClientInfo -> m ()
sync remoteInfo = do
  hastoryClient <- getHastoryClient remoteInfo
  unSyncdEntries <- (fmap . fmap) entityVal readUnsyncdEntries
  unless (null unSyncdEntries) (sendEntriesToServer hastoryClient unSyncdEntries)

getHastoryClient :: (MonadThrow m, MonadUnliftIO m) => RemoteStorageClientInfo -> m HastoryClient
getHastoryClient (RemoteStorageClientInfo baseUrl username password) = do
  eHastoryClient <- liftIO $ mkHastoryClient baseUrl username password
  case eHastoryClient of
    Left _err -> liftIO $ die "Unable to log in to server"
    Right client -> pure client

readUnsyncdEntries :: (MonadThrow m, MonadReader Settings m, MonadUnliftIO m) => m [Entity Entry]
readUnsyncdEntries = runDb $ selectList [EntrySyncWitness ==. Nothing] []

sendEntriesToServer :: (MonadUnliftIO m) => HastoryClient -> [Entry] -> m ()
sendEntriesToServer HastoryClient {..} entries = do
  hostName <- liftIO getHostName
  let syncReq = toSyncRequest entries hostName
  syncResults <-
    liftIO $ runHastoryClient (createEntryClient hastoryClientToken syncReq) hastoryClientEnv
  case syncResults of
    Left _clientError -> error "left"
    Right _ -> pure ()
