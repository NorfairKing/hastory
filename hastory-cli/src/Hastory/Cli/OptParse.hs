{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

module Hastory.Cli.OptParse
  ( combineToInstructions
  , getConfiguration
  , getInstructions
  , envParser
  , runArgumentsParser
  , Instructions(..)
  , Dispatch(..)
  , Settings(..)
  ) where

import Control.Monad
import Data.Hastory.Types
import Data.Maybe (fromMaybe)
import Data.Semigroup ((<>))
import qualified Data.Text as T
import qualified Env
import Hastory.Cli.OptParse.Types
import Options.Applicative
import qualified Options.Applicative.Help.Pretty as OptParseHelp
import Path
import Path.IO (getAppUserDataDir, getHomeDir, resolveDir, resolveDir', resolveFile, resolveFile')
import Servant.Client.Core.Reexport (parseBaseUrl)
import System.Environment (getArgs)
import YamlParse.Applicative hiding (Parser)

getInstructions :: IO Instructions
getInstructions = do
  Arguments cmd flags <- getArguments
  environment <- getEnvironment
  defaultConfigFile <- getAppUserDataDir "hastory" >>= flip resolveFile "hastory.yaml"
  mConfig <- getConfiguration defaultConfigFile flags environment
  combineToInstructions cmd flags environment mConfig

combineToInstructions :: Command -> Flags -> Environment -> Maybe Configuration -> IO Instructions
combineToInstructions cmd Flags {..} Environment {..} mConf =
  Instructions <$> getDispatch <*> getSettings
  where
    getDispatch = pure dispatch
    dispatch =
      case cmd of
        CommandGather _ -> DispatchGather GatherSettings
        CommandGenGatherWrapperScript _ ->
          DispatchGenGatherWrapperScript GenGatherWrapperScriptSettings
        CommandListRecentDirs ListRecentDirFlags {..} ->
          let lrdBypassCache = lrdArgBypassCache <|> envLrdBypassCache <|> mc configLrdBypassCache
           in DispatchListRecentDirs
                ListRecentDirSettings {lrdSetBypassCache = fromMaybe False lrdBypassCache}
        CommandChangeDir ChangeDirFlags {..} ->
          DispatchChangeDir ChangeDirSettings {changeDirSetIdx = changeDirFlagsIdx}
        CommandGenChangeWrapperScript _ ->
          DispatchGenChangeWrapperScript GenChangeWrapperScriptSettings
        CommandSuggestAlias _ -> DispatchSuggestAlias SuggestAliasSettings
    getSettings = do
      home <- getHomeDir
      cacheDir <-
        case flagCacheDir <|> envCacheDir <|> mc configCacheDir of
          Nothing -> resolveDir home ".hastory"
          Just cd -> resolveDir' cd
      pure Settings {setCacheDir = cacheDir}
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
    (Environment <$>
     Env.var
       (pure . Just <=< Env.nonempty)
       "CACHE_DIR"
       (Env.help "the cache directory for hastory" <> Env.def Nothing) <*>
     Env.var
       (pure . Just <=< Env.nonempty)
       "CONFIG_FILE"
       (Env.help "path to a config file" <> Env.def Nothing) <*>
     Env.var
       baseUrlParser
       "STORAGE_SERVER_URL"
       (Env.help "URL of the central storage server" <> Env.def Nothing) <*>
     Env.var
       usernameParser
       "STORAGE_SERVER_USERNAME"
       (Env.help "Username for the central storage server" <> Env.def Nothing) <*>
     Env.var
       (pure . Just <=< Env.nonempty)
       "STORAGE_SERVER_PASSWORD"
       (Env.help "Password for the central storage server" <> Env.def Nothing) <*>
     Env.var
       (fmap Just . Env.auto)
       "BYPASS_CACHE"
       (Env.help "Always recompute the recent directory options" <> Env.def Nothing))
  where
    baseUrlParser unparsedUrl =
      maybe (Left $ Env.UnreadError unparsedUrl) (Right . Just) (parseBaseUrl unparsedUrl)
    usernameParser username =
      maybe (Left $ Env.UnreadError username) (pure . Just) (parseUsername $ T.pack username)

runArgumentsParser :: [String] -> ParserResult Arguments
runArgumentsParser =
  execParserPure
    ParserPrefs
      { prefMultiSuffix = "HASTORY"
      , prefDisambiguate = True
      , prefShowHelpOnError = True
      , prefShowHelpOnEmpty = True
      , prefBacktrack = True
      , prefColumns = 80
      }
    argParser

argParser :: ParserInfo Arguments
argParser = info (helper <*> parseArgs) (fullDesc <> progDesc "Hastory" <> footerDoc footerStr)
  where
    footerStr =
      Just $
      OptParseHelp.string $
      unlines
        [ Env.helpDoc envParser
        , ""
        , "Configuration file format:"
        , T.unpack (prettySchemaDoc @Configuration)
        ]

parseArgs :: Parser Arguments
parseArgs = Arguments <$> parseCommand <*> parseFlags

parseCommand :: Parser Command
parseCommand =
  hsubparser $
  mconcat
    [ command "gather" parseCommandGather
    , command "generate-gather-wrapper-script" parseGenGatherWrapperScript
    , command "change-directory" parseCommandChangeDir
    , command "list-recent-directories" parseCommandListRecentDirs
    , command "generate-change-directory-wrapper-script" parseGenChangeDirectoryWrapperScript
    , command "suggest-alias" parseSuggestAlias
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
    (progDesc "Generate the wrapper script to use 'gather'")

parseCommandChangeDir :: ParserInfo Command
parseCommandChangeDir =
  info
    (CommandChangeDir . ChangeDirFlags <$>
     argument
       auto
       (mconcat
          [ help "The index of the directory to change to, see 'list-recent-directories'"
          , metavar "INT"
          ]))
    (progDesc "Output a directory to change to based on the gathered data.")

parseCommandListRecentDirs :: ParserInfo Command
parseCommandListRecentDirs =
  info
    (CommandListRecentDirs <$>
     (ListRecentDirFlags <$>
      (flag'
         (Just True)
         (mconcat [long "bypass-cache", help "Always recompute the recent directory options"]) <|>
       flag'
         (Just False)
         (mconcat [long "no-bypass-cache", help "Use the recent directory cache when available"]) <|>
       pure Nothing)))
    (progDesc "List the directories that were the working directory most often (recently )")

parseGenChangeDirectoryWrapperScript :: ParserInfo Command
parseGenChangeDirectoryWrapperScript =
  info
    (pure $ CommandGenChangeWrapperScript GenChangeWrapperScriptFlags)
    (progDesc "Generate the wrapper script to use 'change-directory'")

parseSuggestAlias :: ParserInfo Command
parseSuggestAlias =
  info
    (pure $ CommandSuggestAlias SuggestAliasFlags)
    (progDesc "Suggest commands for which the user may want to make aliases.")

parseFlags :: Parser Flags
parseFlags =
  Flags <$>
  optional
    (option
       nonEmptyString
       (mconcat [long "cache-dir", metavar "FILEPATH", help "the cache directory for hastory"])) <*>
  optional
    (option
       nonEmptyString
       (mconcat [long "config-file", metavar "FILEPATH", help "path to a config file"])) <*>
  optional
    (option
       (maybeReader parseBaseUrl)
       (mconcat [long "storage-server-url", metavar "URL", help "URL of the central storage server"])) <*>
  optional
    (option
       (maybeReader (parseUsername . T.pack))
       (mconcat
          [ long "storage-server-username"
          , metavar "TEXT"
          , help "Username for the central storage server"
          ])) <*>
  optional
    (option
       (T.pack <$> str)
       (mconcat
          [ long "storage-server-password"
          , metavar "PASSWORD"
          , help "Password for the central storage server"
          ]))
  where
    nonEmptyString =
      maybeReader $ \s ->
        if null s
          then Nothing
          else Just s
