{-# LANGUAGE OverloadedStrings #-}

module Hastory.Cli.Commands.GenGatherWrapperSpec
  ( spec
  ) where

import TestImport

import Data.Hastory
import Hastory.Cli.Commands.GenGatherWrapper
import Hastory.Cli.OptParse.Types ()
import Servant.Client.Core

spec :: Spec
spec =
  describe "genScript" $ do
    context "remote storage info is NOT provided" $ do
      it "does not include username" $ do
        let genScriptSettings = GenGatherWrapperScriptSettings Nothing
        genScript genScriptSettings `shouldNotContain` "--storage-server-username"
      it "does not include url" $ do
        let genScriptSettings = GenGatherWrapperScriptSettings Nothing
        genScript genScriptSettings `shouldNotContain` "--storage-server-url"
      it "does not include password" $ do
        let genScriptSettings = GenGatherWrapperScriptSettings Nothing
        genScript genScriptSettings `shouldNotContain` "--storage-server-password"
    context "remote storage info IS provided" $
      beforeAll getRemoteStorageInfo $ do
        it "includes username" $ \remoteStorageInfo -> do
          let genScriptSettings = GenGatherWrapperScriptSettings (Just remoteStorageInfo)
          genScript genScriptSettings `shouldContain` "--storage-server-username=Steven"
        it "includes url" $ \remoteStorageInfo -> do
          let genScriptSettings = GenGatherWrapperScriptSettings (Just remoteStorageInfo)
          genScript genScriptSettings `shouldContain` "--storage-server-url=http://api.example.com"
        it "includes password" $ \remoteStorageInfo -> do
          let genScriptSettings = GenGatherWrapperScriptSettings (Just remoteStorageInfo)
          genScript genScriptSettings `shouldContain` "--storage-server-password=Passw0rd"
  where
    getRemoteStorageInfo = do
      url <- parseBaseUrl "api.example.com"
      pure $ RemoteStorageClientInfo url (Username "Steven") "Passw0rd"
