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
  describe "Flags" $ do
    context "cache-dir is provided" $
      it "contains a FilePath" $ do
        let (Success (Arguments _cmd flags)) = runArgumentsParser args
            args = ["gather", "--cache-dir=" <> filePath]
            filePath = "~/hastory"
        flags `shouldBe` emptyFlags {flagCacheDir = Just filePath}
    context "storage-server-url is provided" $
      it "contains a BaseUrl" $ do
        let (Success (Arguments _cmd flags)) = runArgumentsParser args
            args = ["gather", "--storage-server-url=" <> rawUrl]
            rawUrl = "api.example.com"
        url <- parseBaseUrl rawUrl
        flags `shouldBe` emptyFlags {flagStorageServer = Just url}
    context "storage-server-username is provided" $
      it "contains a Username" $ do
        let (Success (Arguments _cmd flags)) = runArgumentsParser args
            args = ["gather", "--storage-server-username=" <> rawUsername]
            rawUsername = "steven"
        username <- parseUsername (T.pack rawUsername)
        flags `shouldBe` emptyFlags {flagStorageUsername = Just username}
    context "storage-server-password is provided" $
      it "contains a password" $ do
        let (Success (Arguments _cmd flags)) = runArgumentsParser args
            args = ["gather", "--storage-server-password=" <> rawPassword]
            rawPassword = "Passw0rd"
        flags `shouldBe` emptyFlags {flagStoragePassword = Just (T.pack rawPassword)}
    context "user provides NO flags" $
      it "is an empty Flag data type" $ do
        let (Success (Arguments _cmd flags)) = runArgumentsParser args
            args = ["gather"]
        flags `shouldBe` emptyFlags

emptyFlags :: Flags
emptyFlags = Flags Nothing Nothing Nothing Nothing
