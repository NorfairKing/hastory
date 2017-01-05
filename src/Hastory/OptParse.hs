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
            CommandQuery -> DispatchQuery

getConfiguration :: Command -> Flags -> IO Configuration
getConfiguration _ _ = pure Configuration

getArguments :: IO Arguments
getArguments = do
    args <- getArgs
    let result = runArgumentsParser args
    handleParseResult result

runArgumentsParser :: [[Char]] -> ParserResult Arguments
runArgumentsParser = execParserPure prefs argParser
  where
    prefs =
        ParserPrefs
        { prefMultiSuffix = "HASTORY"
        , prefDisambiguate = True
        , prefShowHelpOnError = True
        , prefBacktrack = True
        , prefColumns = 80
        }

argParser :: ParserInfo Arguments
argParser = info (helper <*> parseArgs) help
  where
    help = fullDesc <> progDesc description
    description = "Hastory"

parseArgs :: Parser Arguments
parseArgs = (,) <$> parseCommand <*> parseFlags

parseCommand :: Parser Command
parseCommand = hsubparser $ mconcat [command "gather" parseCommandGather, command "query" parseCommandQuery]

parseCommandGather :: ParserInfo Command
parseCommandGather = info parser modifier
  where
    parser = pure CommandGather
    modifier =
        fullDesc <> progDesc "Read a single command on the standard input."

parseCommandQuery :: ParserInfo Command
parseCommandQuery = info parser modifier
  where
    parser = pure CommandQuery
    modifier = fullDesc <> progDesc "Query the gathered data."

parseFlags :: Parser Flags
parseFlags = pure Flags
