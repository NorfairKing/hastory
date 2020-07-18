{-# LANGUAGE OverloadedStrings #-}

module Hastory.Cli.OptParseSpec
  ( spec
  ) where

import qualified Data.Text as T
import Env
import Options.Applicative
import Servant.Client

import TestImport hiding (Failure, Success)

import Data.Hastory
import Hastory.Cli.OptParse
import Hastory.Cli.OptParse.Types

spec :: Spec
spec = describeRunArgumentsParser >> describeEnvParser >> describeCombineToInstructions

describeRunArgumentsParser :: Spec
describeRunArgumentsParser = describe "runArgumentsParser" (describeFlags >> describeCommand)

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
            rawUsername = "hastory"
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

describeEnvParser :: Spec
describeEnvParser =
  describe "envParser" $ do
    context "user provides NO environmental variables" $
      it "parses to an empty Environment" $ do
        let res = Env.parsePure envParser []
        res `shouldBe` Right emptyEnvironment
    context "users provides ALL environmental variables" $
      it "parses to a 'full' Environment" $ do
        let url = "api.example.com"
        parsedUrl <- parseBaseUrl url
        let res = Env.parsePure envParser fullEnvironment
            fullEnvironment =
              [ ("HASTORY_CACHE_DIR", "~/home")
              , ("HASTORY_STORAGE_SERVER_URL", url)
              , ("HASTORY_STORAGE_SERVER_USERNAME", "steven")
              , ("HASTORY_STORAGE_SERVER_PASSWORD", "Passw0rd")
              ]
        res `shouldBe`
          Right
            emptyEnvironment
              { envCacheDir = Just "~/home"
              , envStorageServer = Just parsedUrl
              , envStorageUsername = Just (Username "steven")
              , envStoragePassword = Just "Passw0rd"
              }
    context "users provides SOME environmental variables" $
      it "successfully parses to an Environment" $ do
        let res = Env.parsePure envParser [("HASTORY_CACHE_DIR", "~/home")]
        res `shouldBe` Right emptyEnvironment {envCacheDir = Just "~/home"}
    it "ignores unparseable environmental variable" $ do
      let res = Env.parsePure envParser [("HASTORY_STORAGE_SERVER_URL", "ftp://hoogle.org")]
      res `shouldBe` Right emptyEnvironment

describeCombineToInstructions :: Spec
describeCombineToInstructions =
  describe "combineToInstructions" $
  describe "CommandGenGatherWrapperScript" $ do
    context "Flags does NOT contain all 3 fields of a RemoteStorageClientInfo" $
      it "is DispatchGenGatherWrapperScript with no Nothing" $ do
        let cmd = CommandGenGatherWrapperScript GenGatherWrapperScriptFlags
        Instructions dispatch _settings <-
          combineToInstructions cmd emptyFlags emptyEnvironment emptyConfiguration
        dispatch `shouldBe`
          DispatchGenGatherWrapperScript
            GenGatherWrapperScriptSettings {genGatherWrapperScriptSetRemoteInfo = Nothing}
    context "Flags DOES contain all 3 fields of a RemoteStorageClientInfo" $
      it "is DispatchGenGatherWrapperScript with no Nothing" $ do
        url <- parseBaseUrl "api.example.com"
        username <- parseUsername "hastory"
        let cmd = CommandGenGatherWrapperScript GenGatherWrapperScriptFlags
            flags =
              emptyFlags
                { flagCacheDir = Nothing
                , flagStorageServer = Just url
                , flagStorageUsername = Just username
                , flagStoragePassword = Just password
                }
            password = "Passw0rd"
            remoteInfo =
              RemoteStorageClientInfo
                { remoteStorageClientInfoBaseUrl = url
                , remoteStorageClientInfoUsername = username
                , remoteStorageClientInfoPassword = password
                }
        Instructions dispatch _settings <-
          combineToInstructions cmd flags emptyEnvironment emptyConfiguration
        dispatch `shouldBe`
          DispatchGenGatherWrapperScript
            GenGatherWrapperScriptSettings {genGatherWrapperScriptSetRemoteInfo = Just remoteInfo}

emptyFlags :: Flags
emptyFlags =
  Flags
    { flagCacheDir = Nothing
    , flagStorageServer = Nothing
    , flagStorageUsername = Nothing
    , flagStoragePassword = Nothing
    }

emptyConfiguration :: Configuration
emptyConfiguration = Configuration

emptyEnvironment :: Environment
emptyEnvironment =
  Environment
    { envCacheDir = Nothing
    , envStorageServer = Nothing
    , envStorageUsername = Nothing
    , envStoragePassword = Nothing
    }

isParserFailure :: ParserResult a -> Bool
isParserFailure (Failure _) = True
isParserFailure _ = False
