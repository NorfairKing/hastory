module Hastory.Cli.OptParseSpec
  ( spec
  ) where

import qualified Data.Text as T
import Options.Applicative
import Servant.Client
import TestImport hiding (Success)

import Data.Hastory
import Hastory.Cli.OptParse
import Hastory.Cli.OptParse.Types

spec :: Spec
spec =
  describe "runArgumentsParser" $
  describe "generate-gather-wrapper-script" $ do
    context "user does NOT provided remote server information" $
      it "CommandGenGatherWrapperScript has no payload" $ do
        let cliArgs = ["generate-gather-wrapper-script"]
            Success (Arguments cmd _flag) = runArgumentsParser cliArgs
        cmd `shouldBe` CommandGenGatherWrapperScript Nothing
    context "command-line arguments contains ONLY a url" $
      it "CommandGenGatherWrapperScript has no payload" $ do
        let cliArgs = ["generate-gather-wrapper-script", "--storage-server-url=api.example.com"]
            Success (Arguments cmd _flag) = runArgumentsParser cliArgs
        cmd `shouldBe` CommandGenGatherWrapperScript Nothing
    context "command-line arguments contains ONLY a username" $
      it "CommandGenGatherWrapperScript has no payload" $ do
        let cliArgs = ["generate-gather-wrapper-script", "--storage-server-username=steven"]
            Success (Arguments cmd _flag) = runArgumentsParser cliArgs
        cmd `shouldBe` CommandGenGatherWrapperScript Nothing
    context "command-line arguments contains ONLY a password" $
      it "CommandGenGatherWrapperScript has no payload" $ do
        let cliArgs = ["generate-gather-wrapper-script", "--storage-server-password=Passw0rd"]
            Success (Arguments cmd _flag) = runArgumentsParser cliArgs
        cmd `shouldBe` CommandGenGatherWrapperScript Nothing
    context "command-line arguments contains url, username, AND password" $
      it "CommandGenGatherWrapperScript has correct RemoteStorageClientInfo payload" $ do
        let (simpleUrl, simpleUsername, password) = ("api.example.com", "steven", "Passw0rd")
        url <- parseBaseUrl simpleUrl
        username <- parseUsername (T.pack simpleUsername)
        let remoteStorageInfo = RemoteStorageClientInfo url username (T.pack password)
        let cliArgs =
              [ "generate-gather-wrapper-script"
              , "--storage-server-url=" <> simpleUrl
              , "--storage-server-username=" <> simpleUsername
              , "--storage-server-password=" <> password
              ]
            Success (Arguments cmd _flag) = runArgumentsParser cliArgs
        cmd `shouldBe` CommandGenGatherWrapperScript (Just remoteStorageInfo)
