module Hastory.OptParse
    ( module Hastory.OptParse
    , module Hastory.OptParse.Types
    ) where

import Introduction

import Options.Applicative

import Hastory.OptParse.Types

getInstructions :: IO Instructions
getInstructions = do
    (cmd, flags) <- getArguments
    config <- getConfiguration cmd flags
    combineToInstructions cmd flags config

combineToInstructions :: Command -> Flags -> Configuration -> IO Instructions
combineToInstructions cmd Flags Configuration = pure (d, Settings)
  where
    d =
        case cmd of
            CommandGather -> DispatchGather
            CommandGenGatherWrapperScript -> DispatchGenGatherWrapperScript
            CommandListRecentDirs -> DispatchListRecentDirs
            CommandChangeDir i -> DispatchChangeDir i
            CommandGenChangeWrapperScript -> DispatchGenChangeWrapperScript

getConfiguration :: Command -> Flags -> IO Configuration
getConfiguration _ _ = pure Configuration

getArguments :: IO Arguments
getArguments = do
    args <- getArgs
    let result = runArgumentsParser args
    handleParseResult result

runArgumentsParser :: [[Char]] -> ParserResult Arguments
runArgumentsParser =
    execParserPure
        (ParserPrefs
         { prefMultiSuffix = "HASTORY"
         , prefDisambiguate = True
         , prefShowHelpOnError = True
         , prefBacktrack = True
         , prefColumns = 80
         })
        argParser

argParser :: ParserInfo Arguments
argParser = info (helper <*> parseArgs) (fullDesc <> progDesc "Hastory")

parseArgs :: Parser Arguments
parseArgs = (,) <$> parseCommand <*> parseFlags

parseCommand :: Parser Command
parseCommand =
    hsubparser $
    mconcat
        [ command "gather" parseCommandGather
        , command "generate-gather-wrapper-script" parseGenGatherWrapperScript
        , command "change-directory" parseCommandChangeDir
        , command "list-recent-directories" parseCommandListRecentDirs
        , command
              "generate-change-directory-wrapper-script"
              parseGenChangeDirectoryWrapperScript
        ]

parseCommandGather :: ParserInfo Command
parseCommandGather =
    info
        (pure CommandGather)
        (fullDesc <> progDesc "Read a single command on the standard input.")

parseGenGatherWrapperScript :: ParserInfo Command
parseGenGatherWrapperScript =
    info
        (pure CommandGenGatherWrapperScript)
        (progDesc "Generate the wrapper script to use 'gather'")

parseCommandChangeDir :: ParserInfo Command
parseCommandChangeDir =
    info
        (CommandChangeDir <$>
         argument
             auto
             (mconcat
                  [ help
                        "The index of the directory to change to, see 'list-recent-directories'"
                  , metavar "INT"
                  ]))
        (progDesc "Output a directory to change to based on the gathered data.")

parseCommandListRecentDirs :: ParserInfo Command
parseCommandListRecentDirs =
    info
        (pure CommandListRecentDirs)
        (progDesc
             "List the directories that were the working directory most often (recently )")

parseGenChangeDirectoryWrapperScript :: ParserInfo Command
parseGenChangeDirectoryWrapperScript =
    info
        (pure CommandGenChangeWrapperScript)
        (progDesc "Generate the wrapper script to use 'change-directory'")

parseFlags :: Parser Flags
parseFlags = pure Flags
