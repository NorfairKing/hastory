{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Hastory.Cli.Commands.RegisterSpec
  ( spec,
  )
where

import Hastory.Cli.Commands.Register
import Hastory.Data.Server.DB
import Hastory.Data.Username
import Hastory.Server.TestUtils
import Servant.Client
import TestImport

spec :: Spec
spec =
  serverSpec
    $ describe "register"
    $ it "creates a user on the server"
    $ \ServerInfo {..} -> do
      let username = Username "steven"
      let remoteInfo = RemoteStorage (baseUrl siClientEnv) username "Passw0rd"
      _ <- register (RegisterSettings remoteInfo)
      serverUsers <- runSqlPool (selectList [] []) siPool
      map (userName . entityVal) serverUsers `shouldBe` [username]
