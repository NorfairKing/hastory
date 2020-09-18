{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Hastory.Cli.Commands.SyncSpec
  ( spec
  ) where

import Control.Monad.Reader
import Data.Hastory.Gen ()
import Data.Hastory.Server.TestUtils
import Hastory.Cli.Commands.Sync
import Hastory.Cli.Internal
import Servant.Client
import TestImport

spec :: Spec
spec =
  serverSpec $
  describe "sync" $
  it "sends unsync'd data to the sync server" $ \ServerInfo {..} -> do
    userForm <- generate genValid
    entries <- generate $ vectorOf 2 genValid
    let remoteInfo = RemoteStorageClientInfo (baseUrl siClientEnv) username password
        username = userFormUserName userForm
        password = userFormPassword userForm
    withNewUser siClientEnv userForm $ \_registrationData ->
      withSystemTempDir "local-hastory.db" $ \tmpDir -> do
        let settings = Settings tmpDir
        _ <- createUnsyncdEntries entries settings
        runReaderT (sync remoteInfo) settings
        serverEntries :: [Entity ServerEntry] <- runSqlPool (selectList [] []) siPool
        length serverEntries `shouldBe` 2

createUnsyncdEntries :: [Entry] -> Settings -> IO [Key Entry]
createUnsyncdEntries entries = runReaderT (runDb $ insertMany entries)
