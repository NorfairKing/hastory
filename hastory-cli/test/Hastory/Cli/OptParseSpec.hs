{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Hastory.Cli.OptParseSpec
  ( spec,
  )
where

import qualified Data.ByteString as B
import Env
import Hastory.Cli.OptParse
import Hastory.Data
import Options.Applicative
import Servant.Client
import TestImport hiding (Failure, Success)

spec :: Spec
spec = do
  runArgumentsParserSpec
  envParserSpec
  getConfigurationSpec
  combineToInstructionsSpec

getConfigurationSpec :: Spec
getConfigurationSpec =
  describe "getConfigurationSpec"
    $ around withDefaultConfigFile
    $ do
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
        context "default config file does not exist"
          $ it "has 'empty' configuration"
          $ \defaultConfigFile -> do
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
    context "cache-dir is provided" $ do
      it "contains a FilePath when FilePath is provided" $ do
        let (Success (Arguments _cmd flags)) = runArgumentsParser args
            args = ["gather", "--cache-dir=" <> filePath]
            filePath = "~/hastory"
        flags `shouldBe` emptyFlags {flagCacheDir = Just filePath}
      it "is an error when FilePath is empty" $ do
        let res = runArgumentsParser args
            args = ["gather", "--cache-dir="]
        res `shouldSatisfy` isCliParserFailure
    context "data-dir is provided" $ do
      it "contains a FilePath when FilePath is provided" $ do
        let (Success (Arguments _cmd flags)) = runArgumentsParser args
            args = ["gather", "--data-dir=" <> filePath]
            filePath = "~/hastory"
        flags `shouldBe` emptyFlags {flagDataDir = Just filePath}
      it "is an error when FilePath is empty" $ do
        let res = runArgumentsParser args
            args = ["gather", "--data-dir="]
        res `shouldSatisfy` isCliParserFailure
    context "config-file is provided" $ do
      it "contains a Filepath when FilePath is provided" $ do
        let (Success (Arguments _cmd flags)) = runArgumentsParser args
            args = ["gather", "--config-file=" <> filePath]
            filePath = "~/hastory"
        flags `shouldBe` emptyFlags {flagConfigFile = Just filePath}
      it "is an error when FilePath is empty" $ do
        let res = runArgumentsParser args
            args = ["gather", "--config-file="]
        res `shouldSatisfy` isCliParserFailure
    context "user provides NO flags"
      $ it "is an empty Flags data type"
      $ do
        let (Success (Arguments _cmd flags)) = runArgumentsParser args
            args = ["gather"]
        flags `shouldBe` emptyFlags

describeCommand :: Spec
describeCommand =
  describe "Command" $ do
    context "user provides the 'gather' command"
      $ it "parses to CommandGather"
      $ do
        let (Success (Arguments cmd _flags)) = runArgumentsParser args
            args = ["gather"]
        cmd `shouldBe` CommandGather GatherFlags
    context "user provides the 'generate-gather-wrapper-script' command"
      $ it "parses to CommandGenGatherWrapperScript"
      $ do
        let (Success (Arguments cmd _flags)) = runArgumentsParser args
            args = ["generate-gather-wrapper-script"]
        cmd `shouldBe` CommandGenGatherWrapperScript GenGatherWrapperScriptFlags
    context "user provides the 'change-directory' command" $ do
      it "parses to CommandChangeDir when INT is provided" $ do
        let (Success (Arguments cmd _flags)) = runArgumentsParser args
            args = ["change-directory", "23"]
        cmd `shouldBe` CommandChangeDir ChangeDirFlags {changeDirFlagsIdx = 23}
      it "fails to parse with NO options" $ do
        let res = runArgumentsParser args
            args = ["change-directory"]
        res `shouldSatisfy` isCliParserFailure
      it "fails to parse when value provided is not an INT" $ do
        let res = runArgumentsParser args
            args = ["change-directory", "invalid"]
        res `shouldSatisfy` isCliParserFailure
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
      it "fails to parse when option is invalid" $ do
        let res = runArgumentsParser args
            args = ["list-recent-directories", "--invalid"]
        res `shouldSatisfy` isCliParserFailure
    context "user provides the 'generate-change-directory-wrapper-script' command"
      $ it "parses to CommandGenChangeWrapperScript"
      $ do
        let (Success (Arguments cmd _flags)) = runArgumentsParser args
            args = ["generate-change-directory-wrapper-script"]
        cmd `shouldBe` CommandGenChangeWrapperScript GenChangeWrapperScriptFlags
    context "user provides the 'suggest-alias' command"
      $ it "parses to CommandSuggestAlias"
      $ do
        let (Success (Arguments cmd _flags)) = runArgumentsParser args
            args = ["suggest-alias"]
        cmd `shouldBe` CommandSuggestAlias SuggestAliasFlags
    context "user provides the 'sync' command"
      $ it "parses to CommandSync"
      $ do
        url <- parseBaseUrl "api.google.com"
        let (Success (Arguments cmd _flags)) = runArgumentsParser args
            args =
              [ "sync",
                "--storage-server=api.google.com",
                "--storage-username=steven",
                "--storage-password=letmein"
              ]
            expectedSyncFlags = SyncFlags (Just url) (Just $ Username "steven") (Just "letmein")
        cmd `shouldBe` CommandSync expectedSyncFlags
    context "user provides the 'register' command"
      $ it "parses to CommandRegister"
      $ do
        url <- parseBaseUrl "api.google.com"
        let (Success (Arguments cmd _flags)) = runArgumentsParser args
            args =
              [ "register",
                "--storage-server=api.google.com",
                "--storage-username=steven",
                "--storage-password=letmein"
              ]
            expectedRegisterFlags = RegisterFlags (Just url) (Just $ Username "steven") (Just "letmein")
        cmd `shouldBe` CommandRegister expectedRegisterFlags

envParserSpec :: Spec
envParserSpec =
  describe "envParser" $ do
    context "user provides NO environmental variables"
      $ it "parses to an empty Environment"
      $ do
        let res = Env.parsePure envParser []
        res `shouldBe` Right emptyEnvironment
    context "users provides ALL environmental variables"
      $ it "parses to a full Environment"
      $ do
        let url = "api.example.com"
        parsedUrl <- parseBaseUrl url
        let res = Env.parsePure envParser fullEnvironment
            fullEnvironment =
              [ ("HASTORY_CACHE_DIR", "~/home"),
                ("HASTORY_CONFIG_FILE", "~/home/.hastory.yaml"),
                ("HASTORY_STORAGE_SERVER_URL", url),
                ("HASTORY_STORAGE_SERVER_USERNAME", "steven"),
                ("HASTORY_STORAGE_SERVER_PASSWORD", "Passw0rd"),
                ("HASTORY_BYPASS_CACHE", "True")
              ]
        res
          `shouldBe` Right
            emptyEnvironment
              { envCacheDir = Just "~/home",
                envConfigFile = Just "~/home/.hastory.yaml",
                envStorageServer = Just parsedUrl,
                envStorageUsername = Just (Username "steven"),
                envStoragePassword = Just "Passw0rd",
                envLrdBypassCache = Just True
              }
    context "users provides SOME environmental variables" $ do
      it "successfully parses to an Environment" $ do
        let res = Env.parsePure envParser [("HASTORY_CACHE_DIR", "~/home")]
        res `shouldBe` Right emptyEnvironment {envCacheDir = Just "~/home"}
      context "CACHE_DIR is set in environment" $ do
        it "parses correctly when CACHE_DIR is valid" $ do
          let res = Env.parsePure envParser [("HASTORY_CACHE_DIR", "~/home")]
          res `shouldBe` Right emptyEnvironment {envCacheDir = Just "~/home"}
        it "is an error when CACHE_DIR is invalid" $ do
          let res = Env.parsePure envParser [("HASTORY_CACHE_DIR", "")]
          res `shouldSatisfy` isEnvParserFailure
      context "DATA_DIR is set in environment" $ do
        it "parses correctly when DATA_DIR is valid" $ do
          let res = Env.parsePure envParser [("HASTORY_DATA_DIR", "~/home")]
          res `shouldBe` Right emptyEnvironment {envDataDir = Just "~/home"}
        it "is an error when DATA_DIR is invalid" $ do
          let res = Env.parsePure envParser [("HASTORY_DATA_DIR", "")]
          res `shouldSatisfy` isEnvParserFailure
      context "CONFIG_FILE is set in environment" $ do
        it "parses correctly when CONFIG_FILE is valid" $ do
          let res = Env.parsePure envParser [("HASTORY_CONFIG_FILE", "~/home")]
          res `shouldBe` Right emptyEnvironment {envConfigFile = Just "~/home"}
        it "is an error when CONFIG_FILE is invalid" $ do
          let res = Env.parsePure envParser [("HASTORY_CONFIG_FILE", "")]
          res `shouldSatisfy` isEnvParserFailure
      context "STORAGE_SERVER_URL is set in environment" $ do
        it "parses STORAGE_SERVER_URL correctly" $ do
          let res = Env.parsePure envParser [("HASTORY_STORAGE_SERVER_URL", "api.example.com")]
          url <- parseBaseUrl "api.example.com"
          res `shouldBe` Right emptyEnvironment {envStorageServer = Just url}
        it "is an error when STORAGE_SERVER_URL is invalid" $ do
          let res = Env.parsePure envParser [("HASTORY_STORAGE_SERVER_URL", "ftps://1")]
          res `shouldSatisfy` isEnvParserFailure
      context "STORAGE_SERVER_USERNAME is set in environment" $ do
        it "parses STORAGE_SERVER_USERNAME correctly" $ do
          let res = Env.parsePure envParser [("HASTORY_STORAGE_SERVER_USERNAME", "steven")]
          username <- parseUsername "steven"
          res `shouldBe` Right emptyEnvironment {envStorageUsername = Just username}
        it "is an error when STORAGE_SERVER_USERNAME is invalid" $ do
          let res = Env.parsePure envParser [("HASTORY_STORAGE_SERVER_USERNAME", "s")]
          res `shouldSatisfy` isEnvParserFailure
      context "STORAGE_SERVER_PASSWORD is set in environment" $ do
        it "parses STORAGE_SERVER_PASSWORD correctly" $ do
          let res = Env.parsePure envParser [("HASTORY_STORAGE_SERVER_PASSWORD", "Passw0rd")]
          res `shouldBe` Right emptyEnvironment {envStoragePassword = Just "Passw0rd"}
        it "is an error when STORAGE_SERVER_PASSWORD is invalid" $ do
          let res = Env.parsePure envParser [("HASTORY_STORAGE_SERVER_PASSWORD", "")]
          res `shouldSatisfy` isEnvParserFailure
      context "BYPASS_CACHE is set in environment" $ do
        it "parses True correctly when the env var is set to True" $ do
          let res = Env.parsePure envParser [("HASTORY_BYPASS_CACHE", "True")]
          res `shouldBe` Right emptyEnvironment {envLrdBypassCache = Just True}
        it "parses False correctly when the envvar is not set" $ do
          let res = Env.parsePure envParser [("HASTORY_BYPASS_CACHE", "False")]
          res `shouldBe` Right emptyEnvironment {envLrdBypassCache = Just False}
        it "is an error when HASTORY_BYPASS_CACHE is invalid" $ do
          let res = Env.parsePure envParser [("HASTORY_BYPASS_CACHE", "hello")]
          res `shouldSatisfy` isEnvParserFailure

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
        setCacheDir settings `shouldBe` stevenAbsDir
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
        setCacheDir settings `shouldBe` chrisAbsDir
      it "falls back to Config when Flags / Environment is missing" $ do
        let stevenHomeDir = "/home/steven"
        stevenAbsDir <- resolveDir' stevenHomeDir
        Instructions _ settings <-
          combineToInstructions
            (CommandGenGatherWrapperScript GenGatherWrapperScriptFlags)
            emptyFlags
            emptyEnvironment
            (Just (emptyConfiguration {configCacheDir = Just stevenHomeDir}))
        setCacheDir settings `shouldBe` stevenAbsDir
      it "has default when Configuration / Flags / Environment / Config cache is missing" $ do
        defaultCacheDir <- getXdgDir XdgCache (Just [reldir|hastory|])
        defaultDataDir <- getXdgDir XdgData (Just [reldir|hastory|])
        Instructions _ settings <-
          combineToInstructions
            (CommandGenGatherWrapperScript GenGatherWrapperScriptFlags)
            emptyFlags
            emptyEnvironment
            Nothing
        settings `shouldBe` Settings defaultCacheDir defaultDataDir
    describe "CommandGenGatherWrapperScript"
      $ it "is DispatchGenGatherWrapperScript with GenGatherWrapperScriptSettings"
      $ do
        let cmd = CommandGenGatherWrapperScript GenGatherWrapperScriptFlags
        Instructions dispatch _settings <-
          combineToInstructions cmd emptyFlags emptyEnvironment Nothing
        dispatch `shouldBe` DispatchGenGatherWrapperScript GenGatherWrapperScriptSettings

emptyFlags :: Flags
emptyFlags = Flags {flagCacheDir = Nothing, flagConfigFile = Nothing, flagDataDir = Nothing}

emptyConfiguration :: Configuration
emptyConfiguration =
  Configuration
    { configCacheDir = Nothing,
      configStorageServer = Nothing,
      configStorageUsername = Nothing,
      configStoragePassword = Nothing,
      configLrdBypassCache = Nothing,
      configDataDir = Nothing
    }

emptyEnvironment :: Environment
emptyEnvironment =
  Environment
    { envCacheDir = Nothing,
      envConfigFile = Nothing,
      envStorageServer = Nothing,
      envStorageUsername = Nothing,
      envStoragePassword = Nothing,
      envLrdBypassCache = Nothing,
      envDataDir = Nothing
    }

isCliParserFailure :: ParserResult a -> Bool
isCliParserFailure (Failure _) = True
isCliParserFailure _ = False

isEnvParserFailure :: Either e a -> Bool
isEnvParserFailure (Left _) = True
isEnvParserFailure _ = False
