{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

module Hastory.Cli.OptParse
  ( combineToInstructions,
    getConfiguration,
    getInstructions,
    envParser,
    runArgumentsParser,
    Instructions (..),
    Dispatch (..),
    Settings (..),
  )
where

import Control.Monad
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import qualified Env
import Hastory.Cli.OptParse.Types
import Hastory.Data
import Options.Applicative
import qualified Options.Applicative.Help.Pretty as OptParseHelp
import Path.IO
  ( XdgDirectory (..),
    getXdgDir,
    resolveDir',
    resolveFile,
    resolveFile',
  )
import Servant.Client.Core.Reexport (parseBaseUrl)
import System.Environment (getArgs)
import System.Exit (die)
import YamlParse.Applicative hiding (Parser)

getInstructions :: IO Instructions
getInstructions = do
  Arguments cmd flags <- getArguments
  environment <- getEnvironment
  defaultConfigFile <- getXdgDir XdgConfig (Just [reldir|hastory|]) >>= flip resolveFile "config.yaml"
  mConfig <- getConfiguration defaultConfigFile flags environment
  combineToInstructions cmd flags environment mConfig

combineToInstructions :: Command -> Flags -> Environment -> Maybe Configuration -> IO Instructions
combineToInstructions cmd Flags {..} Environment {..} mConf =
  Instructions <$> getDispatch <*> getSettings
  where
    getDispatch =
      case cmd of
        CommandGather _ -> pure $ DispatchGather GatherSettings
        CommandGenGatherWrapperScript _ ->
          pure $ DispatchGenGatherWrapperScript GenGatherWrapperScriptSettings
        CommandListRecentDirs ListRecentDirFlags {..} ->
          let lrdBypassCache = lrdArgBypassCache <|> envLrdBypassCache <|> mc configLrdBypassCache
           in pure $
                DispatchListRecentDirs
                  ListRecentDirSettings {lrdSetBypassCache = fromMaybe False lrdBypassCache}
        CommandChangeDir ChangeDirFlags {..} ->
          pure $ DispatchChangeDir ChangeDirSettings {changeDirSetIdx = changeDirFlagsIdx}
        CommandGenChangeWrapperScript _ ->
          pure $ DispatchGenChangeWrapperScript GenChangeWrapperScriptSettings
        CommandSuggestAlias _ -> pure $ DispatchSuggestAlias SuggestAliasSettings
        CommandSync syncFlags -> DispatchSync . SyncSettings <$> getRemoteStorage syncFlags
        CommandRegister registerFlags -> DispatchRegister . RegisterSettings <$> getRemoteStorage registerFlags
    getRemoteStorage :: RemoteStorageFlags -> IO RemoteStorage
    getRemoteStorage RemoteStorageFlags {..} = do
      remoteStorageBaseUrl <-
        case remoteStorageFlagsServer <|> envStorageServer <|> mc configStorageServer of
          Nothing -> die "Storage server not configured."
          Just baseUrl -> pure baseUrl
      remoteStorageUsername <-
        case remoteStorageFlagsUsername <|> envStorageUsername <|> mc configStorageUsername of
          Nothing -> die "Username not configured."
          Just username -> pure username
      remoteStoragePassword <-
        case remoteStorageFlagsPassword <|> envStoragePassword <|> mc configStoragePassword of
          Nothing -> die "Password not configured."
          Just pw -> pure pw
      pure RemoteStorage {..}
    getSettings = do
      cacheDir <-
        case flagCacheDir <|> envCacheDir <|> mc configCacheDir of
          Nothing -> getXdgDir XdgCache (Just [reldir|hastory|])
          Just cd -> resolveDir' cd
      dataDir <-
        case flagDataDir <|> envDataDir <|> mc configDataDir of
          Nothing -> getXdgDir XdgData (Just [reldir|hastory|])
          Just cd -> resolveDir' cd
      pure Settings {setCacheDir = cacheDir, setDataDir = dataDir}
    mc :: (Configuration -> Maybe a) -> Maybe a
    mc func = mConf >>= func

getConfiguration :: Path Abs File -> Flags -> Environment -> IO (Maybe Configuration)
getConfiguration defaultConfigFile Flags {..} Environment {..} =
  case flagConfigFile <|> envConfigFile of
    Just userProvidedPath -> resolveFile' userProvidedPath >>= readConfigFile
    Nothing -> readConfigFile defaultConfigFile

getArguments :: IO Arguments
getArguments = do
  args <- getArgs
  let result = runArgumentsParser args
  handleParseResult result

getEnvironment :: IO Environment
getEnvironment = Env.parse (Env.header "hastory") envParser

envParser :: Env.Parser Env.Error Environment
envParser =
  Env.prefixed
    "HASTORY_"
    ( Environment
        <$> Env.var
          (pure . Just <=< Env.nonempty)
          "CACHE_DIR"
          (Env.help "The cache directory." <> Env.def Nothing)
        <*> Env.var
          (pure . Just <=< Env.nonempty)
          "CONFIG_FILE"
          (Env.help "Path to a config file." <> Env.def Nothing)
        <*> Env.var
          baseUrlParser
          "STORAGE_SERVER_URL"
          (Env.help "URL of the sync server." <> Env.def Nothing)
        <*> Env.var
          usernameParser
          "STORAGE_SERVER_USERNAME"
          (Env.help "Username for the sync server." <> Env.def Nothing)
        <*> Env.var
          (pure . Just <=< Env.nonempty)
          "STORAGE_SERVER_PASSWORD"
          (Env.help "Password for the sync server." <> Env.def Nothing)
        <*> Env.var
          (fmap Just . Env.auto)
          "BYPASS_CACHE"
          (Env.help "Always recompute the recent directory options." <> Env.def Nothing)
        <*> Env.var
          (pure . Just <=< Env.nonempty)
          "DATA_DIR"
          (Env.help "Data directory for hastory." <> Env.def Nothing)
    )
  where
    baseUrlParser unparsedUrl =
      maybe (Left $ Env.UnreadError unparsedUrl) (Right . Just) (parseBaseUrl unparsedUrl)
    usernameParser username =
      maybe (Left $ Env.UnreadError username) (pure . Just) (parseUsername $ T.pack username)

runArgumentsParser :: [String] -> ParserResult Arguments
runArgumentsParser =
  execParserPure
    ( defaultPrefs
        { prefShowHelpOnError = True,
          prefShowHelpOnEmpty = True
        }
    )
    argParser

argParser :: ParserInfo Arguments
argParser = info (helper <*> parseArgs) (fullDesc <> progDesc "Hastory" <> footerDoc footerStr)
  where
    footerStr =
      Just $
        OptParseHelp.string $
          unlines
            [ Env.helpDoc envParser,
              "",
              "Configuration file format:",
              T.unpack (prettySchemaDoc @Configuration)
            ]

parseArgs :: Parser Arguments
parseArgs = Arguments <$> parseCommand <*> parseFlags

parseCommand :: Parser Command
parseCommand =
  hsubparser $
    mconcat
      [ command "gather" parseCommandGather,
        command "generate-gather-wrapper-script" parseGenGatherWrapperScript,
        command "change-directory" parseCommandChangeDir,
        command "list-recent-directories" parseCommandListRecentDirs,
        command "generate-change-directory-wrapper-script" parseGenChangeDirectoryWrapperScript,
        command "suggest-alias" parseSuggestAlias,
        command "sync" parseSync,
        command "register" parseRegister
      ]

parseCommandGather :: ParserInfo Command
parseCommandGather =
  info
    (pure $ CommandGather GatherFlags)
    (fullDesc <> progDesc "Read a single command on the standard input.")

parseGenGatherWrapperScript :: ParserInfo Command
parseGenGatherWrapperScript =
  info
    (pure $ CommandGenGatherWrapperScript GenGatherWrapperScriptFlags)
    (progDesc "Generate the wrapper script to use 'gather'.")

parseCommandChangeDir :: ParserInfo Command
parseCommandChangeDir =
  info
    ( CommandChangeDir . ChangeDirFlags
        <$> argument
          auto
          ( mconcat
              [ help "The index of the directory to change to, see 'list-recent-directories'.",
                metavar "INT"
              ]
          )
    )
    (progDesc "Output a directory to change to based on the gathered data.")

parseCommandListRecentDirs :: ParserInfo Command
parseCommandListRecentDirs =
  info
    ( CommandListRecentDirs
        <$> ( ListRecentDirFlags
                <$> ( flag'
                        (Just True)
                        (mconcat [long "bypass-cache", help "Always recompute the recent directory options."])
                        <|> flag'
                          (Just False)
                          (mconcat [long "no-bypass-cache", help "Use the recent directory cache when available."])
                        <|> pure Nothing
                    )
            )
    )
    (progDesc "List the directories that were the working directory most often / recently.")

parseGenChangeDirectoryWrapperScript :: ParserInfo Command
parseGenChangeDirectoryWrapperScript =
  info
    (pure $ CommandGenChangeWrapperScript GenChangeWrapperScriptFlags)
    (progDesc "Generate the wrapper script to use 'change-directory'.")

parseSuggestAlias :: ParserInfo Command
parseSuggestAlias =
  info
    (pure $ CommandSuggestAlias SuggestAliasFlags)
    (progDesc "Suggest commands for which the user may want to make aliases.")

parseSync :: ParserInfo Command
parseSync = info (CommandSync <$> remoteStorageParser) (progDesc "Sync the local database with a remote server.")

parseRegister :: ParserInfo Command
parseRegister = info (CommandRegister <$> remoteStorageParser) (progDesc "Register with a remote server.")

remoteStorageParser :: Parser RemoteStorageFlags
remoteStorageParser = RemoteStorageFlags <$> remoteStorageFlagServerParser <*> remoteStorageFlagUsernameParser <*> remoteStorageFlagPasswordParser
  where
    remoteStorageFlagServerParser =
      optional $
        option
          (maybeReader parseBaseUrl)
          (long "storage-server" <> help "Remote storage url." <> metavar "URL")
    remoteStorageFlagUsernameParser =
      optional $
        option
          (maybeReader $ parseUsername . T.pack)
          (long "storage-username" <> help "Remote storage username." <> metavar "USERNAME")
    remoteStorageFlagPasswordParser =
      optional $
        strOption (long "storage-password" <> help "Remote storage password." <> metavar "PASSWORD")

parseFlags :: Parser Flags
parseFlags =
  Flags
    <$> optional
      ( option
          nonEmptyString
          (mconcat [long "cache-dir", metavar "FILEPATH", help "The cache directory for hastory."])
      )
    <*> optional
      ( option
          nonEmptyString
          (mconcat [long "config-file", metavar "FILEPATH", help "Path to a config file."])
      )
    <*> optional
      ( option
          nonEmptyString
          (mconcat [long "data-dir", metavar "FILEPATH", help "The data directory for hastory."])
      )
  where
    nonEmptyString =
      maybeReader $ \s ->
        if null s
          then Nothing
          else Just s
