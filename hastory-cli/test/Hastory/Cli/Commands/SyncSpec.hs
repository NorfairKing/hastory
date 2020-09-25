{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Hastory.Cli.Commands.SyncSpec
  ( spec
  ) where

import Control.Monad.Reader
import Data.Hastory.Gen ()
import Data.Hastory.Server.TestUtils
import Data.Hastory.Server.Utils
import Hastory.Cli.Commands.Sync
import Hastory.Cli.Internal
import Servant.Client
import TestImport

spec :: Spec
spec =
  serverSpec $
  describe "sync" $ do
    it "sends unsync'd data to the sync server" $ \ServerInfo {..} ->
      forAllValid $ \userForm ->
        forAllValid $ \(entryOne, entryTwo) -> do
          let remoteInfo = RemoteStorageClientInfo (baseUrl siClientEnv) username password
              username = userFormUserName userForm
              password = userFormPassword userForm
          withNewUser siClientEnv userForm $ \_registrationData ->
            withSystemTempDir "local-hastory" $ \tmpDir -> do
              let settings = Settings tmpDir
                  entries = map nullifySyncWitness [entryOne, entryTwo]
              _ <- createUnsyncdEntries entries settings
              runReaderT (sync remoteInfo) settings
              serverEntries :: [Entity ServerEntry] <- runSqlPool (selectList [] []) siPool
              length serverEntries `shouldBe` 2
    it "fetches new entries from the sync server" $ \ServerInfo {..} ->
      forAllValid $ \userForm ->
        forAllValid $ \(entryOne, entryTwo) -> do
          let remoteStorage = RemoteStorageClientInfo (baseUrl siClientEnv) username password
              username = userFormUserName userForm
              password = userFormPassword userForm
          withNewUser siClientEnv userForm $ \(userId, _token) ->
            withSystemTempDir "local-hastory" $ \tmpDir -> do
              let entries = map nullifySyncWitness [entryOne, entryTwo]
                  serverEntries = map (toServerEntry userId "localhost") entries
              _ <- runSqlPool (insertMany serverEntries) siPool
              let settings = Settings tmpDir
              _ <- runReaderT (sync remoteStorage) settings
              localEntities <- runReaderT (runDb $ selectList [] [Desc EntrySyncWitness]) settings
              serverEntities <- runSqlPool (selectList [] [Desc ServerEntryId]) siPool
              length localEntities `shouldBe` 2
              length serverEntities `shouldBe` 2
              map entityVal localEntities `shouldBe` map toEntry serverEntities
    it "updates local entries when syncing" $ \ServerInfo {..} ->
      forAllValid $ \userForm ->
        forAllValid $ \entry -> do
          let remote = RemoteStorageClientInfo (baseUrl siClientEnv) username password
              username = userFormUserName userForm
              password = userFormPassword userForm
          withNewUser siClientEnv userForm $ \(_userId, _token) ->
            withSystemTempDir "local-hastory" $ \tmpDir -> do
              let set = Settings tmpDir
                  entries = map nullifySyncWitness [entry]
              _ <- createUnsyncdEntries entries set
              _ <- runReaderT (sync remote) set
              localEntities :: [Entity Entry] <- runReaderT (runDb $ selectList [] []) set
              length localEntities `shouldBe` 1

createUnsyncdEntries :: [Entry] -> Settings -> IO [Key Entry]
createUnsyncdEntries entries = runReaderT (runDb $ insertMany entries)

nullifySyncWitness :: Entry -> Entry
nullifySyncWitness entry = entry {entrySyncWitness = Nothing}
