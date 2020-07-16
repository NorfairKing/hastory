{-# LANGUAGE RecordWildCards #-}

module Hastory.Cli.OptParse
  ( combineToInstructions
  , getInstructions
  , runArgumentsParser
  , Instructions(..)
  , Dispatch(..)
  , Settings(..)
  ) where

import Data.Maybe (fromMaybe)
import Data.Semigroup ((<>))
import qualified Data.Text as T
import Options.Applicative
import Path.IO (getHomeDir, resolveDir, resolveDir')
import Servant.Client.Core.Reexport (parseBaseUrl)
import System.Environment (getArgs)

import Data.Hastory.Types
import Hastory.Cli.OptParse.Types

getInstructions :: IO Instructions
getInstructions = do
  Arguments cmd flags <- getArguments
  config <- getConfiguration cmd flags
  combineToInstructions cmd flags config

combineToInstructions :: Command -> Flags -> Configuration -> IO Instructions
combineToInstructions cmd Flags {..} Configuration = Instructions <$> getDispatch <*> getSettings
  where
    getDispatch = pure dispatch
    dispatch =
      case cmd of
        CommandGather _ -> DispatchGather GatherSettings
        CommandGenGatherWrapperScript _ ->
          DispatchGenGatherWrapperScript
            GenGatherWrapperScriptSettings
              {genGatherWrapperScriptSetRemoteInfo = mbRemoteStorageClientInfo}
        CommandListRecentDirs ListRecentDirFlags {..} ->
          DispatchListRecentDirs
            ListRecentDirSettings {lrdSetBypassCache = fromMaybe False lrdArgBypassCache}
        CommandChangeDir ChangeDirFlags {..} ->
          DispatchChangeDir ChangeDirSettings {changeDirSetIdx = changeDirFlagsIdx}
        CommandGenChangeWrapperScript _ ->
          DispatchGenChangeWrapperScript GenChangeWrapperScriptSettings
        CommandSuggestAlias _ -> DispatchSuggestAlias SuggestAliasSettings
    getSettings = do
      home <- getHomeDir
      cacheDir <-
        case flagCacheDir of
          Nothing -> resolveDir home ".hastory"
          Just fcd -> resolveDir' fcd
      pure Settings {setCacheDir = cacheDir, remoteStorageClientInfo = mbRemoteStorageClientInfo}
    mbRemoteStorageClientInfo =
      RemoteStorageClientInfo <$> flagStorageServer <*> flagStorageUsername <*> flagStoragePassword

getConfiguration :: Command -> Flags -> IO Configuration
getConfiguration _ _ = pure Configuration

getArguments :: IO Arguments
getArguments = do
  args <- getArgs
  let result = runArgumentsParser args
  handleParseResult result

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
argParser = info (helper <*> parseArgs) (fullDesc <> progDesc "Hastory")

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
         (mconcat [long "no-bypass-cache", help "Use the recent directory cache when available."]) <|>
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
    (option str (mconcat [long "cache-dir", metavar "DIR", help "the cache directory for hastory"])) <*>
  optional
    (option
       (maybeReader parseBaseUrl)
       (mconcat [long "storage-server-url", metavar "URL", help "URL of the central storage server"])) <*>
  optional
    (option
       (maybeReader (parseUsername . T.pack))
       (mconcat
          [ long "storage-server-username"
          , metavar "USERNAME"
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
