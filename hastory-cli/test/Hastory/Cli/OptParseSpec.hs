module Hastory.Cli.OptParseSpec
  ( spec
  ) where

import qualified Data.Text as T
import Options.Applicative
import Servant.Client
import TestImport hiding (Failure, Success)

import Data.Hastory
import Hastory.Cli.OptParse
import Hastory.Cli.OptParse.Types

spec :: Spec
spec = describe "runArgumentsParser" (describeFlags >> describeCommand)

describeFlags :: Spec
describeFlags =
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

describeCommand :: Spec
describeCommand =
  describe "Command" $ do
    context "user provides the 'gather' command" $
      it "parses to CommandGather" $ do
        let (Success (Arguments cmd _flags)) = runArgumentsParser args
            args = ["gather"]
        cmd `shouldBe` CommandGather GatherFlags
    context "user provides the 'generate-gather-wrapper-script' command" $
      it "parses to CommandGenGatherWrapperScript" $ do
        let (Success (Arguments cmd _flags)) = runArgumentsParser args
            args = ["generate-gather-wrapper-script"]
        cmd `shouldBe` CommandGenGatherWrapperScript GenGatherWrapperScriptFlags
    context "user provides the 'change-directory' command" $ do
      it "fails to parse when INT is NOT provided" $ do
        let res = runArgumentsParser args
            args = ["change-directory"]
        res `shouldSatisfy` isParserFailure
      it "parses to CommandChangeDir when INT is provided" $ do
        let (Success (Arguments cmd _flags)) = runArgumentsParser args
            args = ["change-directory", "23"]
        cmd `shouldBe` CommandChangeDir ChangeDirFlags {changeDirFlagsIdx = 23}
    context "user provides the 'list-recent-directories' command" $ do
      it "parses to CommandListRecentDirs without any options" $ do
        let (Success (Arguments cmd _flags)) = runArgumentsParser args
            args = ["list-recent-directories"]
        cmd `shouldBe` CommandListRecentDirs ListRecentDirFlags {lrdArgBypassCache = Nothing}
      it "parses to CommandListRecentDirs with bypass-cache" $ do
        let (Success (Arguments cmd _flags)) = runArgumentsParser args
            args = ["list-recent-directories", "--bypass-cache"]
        cmd `shouldBe` CommandListRecentDirs ListRecentDirFlags {lrdArgBypassCache = Just True}
      it "parses to CommandListRecentDirs with bypass-cache" $ do
        let (Success (Arguments cmd _flags)) = runArgumentsParser args
            args = ["list-recent-directories", "--no-bypass-cache"]
        cmd `shouldBe` CommandListRecentDirs ListRecentDirFlags {lrdArgBypassCache = Just False}

emptyFlags :: Flags
emptyFlags = Flags Nothing Nothing Nothing Nothing

isParserFailure :: ParserResult a -> Bool
isParserFailure (Failure _) = True
isParserFailure _ = False
