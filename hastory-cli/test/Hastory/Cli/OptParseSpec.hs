{-# LANGUAGE OverloadedStrings #-}

module Hastory.Cli.OptParseSpec
  ( spec
  ) where

import qualified Data.Text as T
import Env
import Options.Applicative
import Servant.Client

import qualified Data.ByteString as B
import TestImport hiding (Failure, Success)

import Data.Hastory
import Hastory.Cli.OptParse

spec :: Spec
spec = do
  runArgumentsParserSpec
  envParserSpec
  getConfigurationSpec
  combineToInstructionsSpec

getConfigurationSpec :: Spec
getConfigurationSpec =
  describe "getConfigurationSpec" $
  around withDefaultConfigFile $ do
    it "prefers Flags over Environment file" $ \defaultConfigFile -> do
      let contentsOfFileInFlags = "url: flag.example.com"
      withFile contentsOfFileInFlags $ \path -> do
        let flags = emptyFlags {flagConfigFile = Just (toFilePath path)}
            environment = emptyEnvironment {envConfigFile = Just "~/randomUser"}
        url <- parseBaseUrl "flag.example.com"
        mConf <- getConfiguration defaultConfigFile flags environment
        mConf `shouldBe` Just (emptyConfiguration {configStorageServer = Just url})
    it "prefers Environment over default file" $ \defaultConfigFile -> do
      let contentsOfFileInEnv = "url: environment.example.com"
      withFile contentsOfFileInEnv $ \path -> do
        let flags = emptyFlags
            environment = emptyEnvironment {envConfigFile = Just (toFilePath path)}
        url <- parseBaseUrl "environment.example.com"
        mConf <- getConfiguration defaultConfigFile flags environment
        mConf `shouldBe` Just (emptyConfiguration {configStorageServer = Just url})
    context "Flags and Environment do NOT specify a config" $ do
      context "default config file exists" $ do
        it "uses the default config file" $ \defaultConfigFile -> do
          let defaultConfigContents = "url: default.example.com"
          B.writeFile (toFilePath defaultConfigFile) defaultConfigContents
          mConf <- getConfiguration defaultConfigFile emptyFlags emptyEnvironment
          url <- parseBaseUrl "default.example.com"
          mConf `shouldBe` Just (emptyConfiguration {configStorageServer = Just url})
        it "does not parse malformed config files" $ \defaultConfigFile -> do
          let defaultConfigContents = "url: 1"
          B.writeFile (toFilePath defaultConfigFile) defaultConfigContents
          getConfiguration defaultConfigFile emptyFlags emptyEnvironment `shouldThrow` anyException
      context "default config file does not exist" $
        it "has 'empty' configuration" $ \defaultConfigFile -> do
          mConf <- getConfiguration defaultConfigFile emptyFlags emptyEnvironment
          mConf `shouldBe` Nothing

type ConfigFileContents = B.ByteString

withFile :: ConfigFileContents -> (Path Abs File -> Expectation) -> Expectation
withFile contents f =
  withSystemTempDir "hastory.yaml" $ \tmpDir -> do
    path <- resolveFile tmpDir "hastory.yaml"
    B.writeFile (toFilePath path) contents
    f path

withDefaultConfigFile :: (Path Abs File -> Expectation) -> Expectation
withDefaultConfigFile f =
  withSystemTempDir "hastory.yaml" $ \tmpDir -> do
    path <- resolveFile tmpDir "hastory.config"
    f path

runArgumentsParserSpec :: Spec
runArgumentsParserSpec = describe "runArgumentsParser" (describeFlags >> describeCommand)

describeFlags :: Spec
describeFlags =
  describe "Flags" $ do
    context "cache-dir is provided" $
      it "contains a FilePath" $ do
        let (Success (Arguments _cmd flags)) = runArgumentsParser args
            args = ["gather", "--cache-dir=" <> filePath]
            filePath = "~/hastory"
        flags `shouldBe` emptyFlags {flagCacheDir = Just filePath}
    context "config-file is provided" $
      it "contains a Filepath" $ do
        let (Success (Arguments _cmd flags)) = runArgumentsParser args
            args = ["gather", "--config-file=" <> filePath]
            filePath = "~/hastory"
        flags `shouldBe` emptyFlags {flagConfigFile = Just filePath}
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
      it "is an empty Flags data type" $ do
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
      it "parses to CommandListRecentDirs with --no-bypass-cache" $ do
        let (Success (Arguments cmd _flags)) = runArgumentsParser args
            args = ["list-recent-directories", "--no-bypass-cache"]
        cmd `shouldBe` CommandListRecentDirs ListRecentDirFlags {lrdArgBypassCache = Just False}

envParserSpec :: Spec
envParserSpec =
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
              , ("HASTORY_CONFIG_FILE", "~/home/.hastory.yaml")
              , ("HASTORY_STORAGE_SERVER_URL", url)
              , ("HASTORY_STORAGE_SERVER_USERNAME", "steven")
              , ("HASTORY_STORAGE_SERVER_PASSWORD", "Passw0rd")
              ]
        res `shouldBe`
          Right
            emptyEnvironment
              { envCacheDir = Just "~/home"
              , envConfigFile = Just "~/home/.hastory.yaml"
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

combineToInstructionsSpec :: Spec
combineToInstructionsSpec =
  describe "combineToInstructions" $ do
    context "setCacheDir" $ do
      it "prefers Flags over Environment" $ do
        let flags = emptyFlags {flagCacheDir = Just stevenHomeDir}
            stevenHomeDir = "/home/steven"
            env = emptyEnvironment {envCacheDir = Just "/home/chris"}
        stevenAbsDir <- resolveDir' stevenHomeDir
        Instructions _ settings <-
          combineToInstructions
            (CommandGenGatherWrapperScript GenGatherWrapperScriptFlags)
            flags
            env
            Nothing
        settings `shouldBe` Settings stevenAbsDir Nothing
      it "falls back to Environment cache if Flags cache is missing" $ do
        let flags = emptyFlags
            env = emptyEnvironment {envCacheDir = Just chrisHomeDir}
            chrisHomeDir = "/home/chris"
        chrisAbsDir <- resolveDir' chrisHomeDir
        Instructions _ settings <-
          combineToInstructions
            (CommandGenGatherWrapperScript GenGatherWrapperScriptFlags)
            flags
            env
            Nothing
        settings `shouldBe` Settings chrisAbsDir Nothing
      it "falls back to Config when Flags / Environment is missing" $ do
        let stevenHomeDir = "/home/steven"
        stevenAbsDir <- resolveDir' stevenHomeDir
        Instructions _ settings <-
          combineToInstructions
            (CommandGenGatherWrapperScript GenGatherWrapperScriptFlags)
            emptyFlags
            emptyEnvironment
            (Just (emptyConfiguration {configCacheDir = Just stevenHomeDir}))
        settings `shouldBe` Settings stevenAbsDir Nothing
      it "has default when Configuration / Flags / Environment / Config cache is missing" $ do
        defaultCacheDir <- getHomeDir >>= flip resolveDir ".hastory"
        Instructions _ settings <-
          combineToInstructions
            (CommandGenGatherWrapperScript GenGatherWrapperScriptFlags)
            emptyFlags
            emptyEnvironment
            Nothing
        settings `shouldBe` Settings defaultCacheDir Nothing
    context "remoteStorageClientInfo" $ do
      it "prefers Flags over Environment" $ do
        let flagUsername = Username "flagUser"
            flagPassword = "flagPassword"
            envUsername = Username "envUser"
        flagBaseUrl <- parseBaseUrl "flag.example.com"
        envBaseUrl <- parseBaseUrl "env.example.com"
        let flags =
              emptyFlags
                { flagStorageServer = Just flagBaseUrl
                , flagStorageUsername = Just flagUsername
                , flagStoragePassword = Just flagPassword
                }
            env =
              emptyEnvironment
                { envStorageServer = Just envBaseUrl
                , envStorageUsername = Just envUsername
                , envStoragePassword = Just "envPassword"
                }
        Instructions _ settings <-
          combineToInstructions
            (CommandGenGatherWrapperScript GenGatherWrapperScriptFlags)
            flags
            env
            Nothing
        remoteStorageClientInfo settings `shouldBe`
          Just (RemoteStorageClientInfo flagBaseUrl flagUsername flagPassword)
      it "combines Flags, Environment, and Configuration correctly" $ do
        flagBaseUrl <- parseBaseUrl "flag.example.com"
        let envUsername = Username "envUser"
            confPassword = "Passw0rd"
        let flags = emptyFlags {flagStorageServer = Just flagBaseUrl}
            env = emptyEnvironment {envStorageUsername = Just envUsername}
            mConf = Just (emptyConfiguration {configStoragePassword = Just "Passw0rd"})
        Instructions _ settings <-
          combineToInstructions
            (CommandGenGatherWrapperScript GenGatherWrapperScriptFlags)
            flags
            env
            mConf
        remoteStorageClientInfo settings `shouldBe`
          Just (RemoteStorageClientInfo flagBaseUrl envUsername confPassword)
      it "is nothing when Flags / Environment / Configuration do not contains full remote data" $
        -- N.B. URL is missing
       do
        let flags = emptyFlags {flagStoragePassword = Just "flagPassword"}
            env = emptyEnvironment {envStorageUsername = Just (Username "envUsername")}
        Instructions _ settings <-
          combineToInstructions
            (CommandGenGatherWrapperScript GenGatherWrapperScriptFlags)
            flags
            env
            Nothing
        remoteStorageClientInfo settings `shouldBe` Nothing
    describe "CommandGenGatherWrapperScript" $ do
      context "Flags contains all 3 fields of a RemoteStorageClientInfo" $
        it "is DispatchGenGatherWrapperScript with remote storage info" $ do
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
            combineToInstructions cmd flags emptyEnvironment Nothing
          dispatch `shouldBe`
            DispatchGenGatherWrapperScript
              GenGatherWrapperScriptSettings {genGatherWrapperScriptSetRemoteInfo = Just remoteInfo}
      context "Environment contains all 3 fields of a RemoteStorageClientInfo" $
        it "is DispatchGenGatherWrapperScript with remote storage info" $ do
          url <- parseBaseUrl "api.example.com"
          username <- parseUsername "hastory"
          let cmd = CommandGenGatherWrapperScript GenGatherWrapperScriptFlags
              env =
                emptyEnvironment
                  { envCacheDir = Nothing
                  , envStorageServer = Just url
                  , envStorageUsername = Just username
                  , envStoragePassword = Just password
                  }
              password = "Passw0rd"
              remoteInfo =
                RemoteStorageClientInfo
                  { remoteStorageClientInfoBaseUrl = url
                  , remoteStorageClientInfoUsername = username
                  , remoteStorageClientInfoPassword = password
                  }
          Instructions dispatch _settings <- combineToInstructions cmd emptyFlags env Nothing
          dispatch `shouldBe`
            DispatchGenGatherWrapperScript
              GenGatherWrapperScriptSettings {genGatherWrapperScriptSetRemoteInfo = Just remoteInfo}
      context "Configuration contains all 3 fields of a RemoteStorageClientInfo" $
        it "is DispatchGenGatherWrapperScript with remote storage info" $ do
          url <- parseBaseUrl "api.example.com"
          username <- parseUsername "hastory"
          let cmd = CommandGenGatherWrapperScript GenGatherWrapperScriptFlags
              mConf =
                Just
                  (emptyConfiguration
                     { configCacheDir = Nothing
                     , configStorageServer = Just url
                     , configStorageUsername = Just username
                     , configStoragePassword = Just password
                     })
              password = "Passw0rd"
              remoteInfo =
                RemoteStorageClientInfo
                  { remoteStorageClientInfoBaseUrl = url
                  , remoteStorageClientInfoUsername = username
                  , remoteStorageClientInfoPassword = password
                  }
          Instructions dispatch _settings <-
            combineToInstructions cmd emptyFlags emptyEnvironment mConf
          dispatch `shouldBe`
            DispatchGenGatherWrapperScript
              GenGatherWrapperScriptSettings {genGatherWrapperScriptSetRemoteInfo = Just remoteInfo}

emptyFlags :: Flags
emptyFlags =
  Flags
    { flagCacheDir = Nothing
    , flagConfigFile = Nothing
    , flagStorageServer = Nothing
    , flagStorageUsername = Nothing
    , flagStoragePassword = Nothing
    }

emptyConfiguration :: Configuration
emptyConfiguration =
  Configuration
    { configCacheDir = Nothing
    , configStorageServer = Nothing
    , configStorageUsername = Nothing
    , configStoragePassword = Nothing
    }

emptyEnvironment :: Environment
emptyEnvironment =
  Environment
    { envCacheDir = Nothing
    , envConfigFile = Nothing
    , envStorageServer = Nothing
    , envStorageUsername = Nothing
    , envStoragePassword = Nothing
    }

isParserFailure :: ParserResult a -> Bool
isParserFailure (Failure _) = True
isParserFailure _ = False
