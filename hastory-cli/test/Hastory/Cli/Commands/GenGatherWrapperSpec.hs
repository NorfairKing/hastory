{-# LANGUAGE OverloadedStrings #-}

module Hastory.Cli.Commands.GenGatherWrapperSpec
  ( spec
  ) where

import TestImport

import Data.Hastory
import Hastory.Cli.Commands.GenGatherWrapper
import Hastory.Cli.OptParse.Types
import Servant.Client.Core

spec :: Spec
spec =
  describe "genScript" $ do
    context "remote storage info is NOT provided" $ do
      it "does not include username" $
        genScript Nothing `shouldNotContain` "--storage-server-username"
      it "does not include url" $ genScript Nothing `shouldNotContain` "--storage-server-url"
      it "does not include password" $
        genScript Nothing `shouldNotContain` "--storage-server-password"
    context "remote storage info IS provided" $
      beforeAll getRemoteStorageInfo $ do
        it "includes username" $ \remoteStorageInfo ->
          genScript (Just remoteStorageInfo) `shouldContain` "--storage-server-username=Steven"
        it "includes url" $ \remoteStorageInfo ->
          genScript (Just remoteStorageInfo) `shouldContain`
          "--storage-server-url=http://api.example.com"
        it "includes password" $ \remoteStorageInfo ->
          genScript (Just remoteStorageInfo) `shouldContain` "--storage-server-password=Passw0rd"
  where
    getRemoteStorageInfo = do
      url <- parseBaseUrl "api.example.com"
      pure $ RemoteStorageClientInfo url (Username "Steven") "Passw0rd"
