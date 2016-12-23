module Hastory.OptParse
    ( module Hastory.OptParse
    , module Hastory.OptParse.Types
    ) where

import           Introduction

import           Options.Applicative

import           Hastory.OptParse.Types

getInstructions :: IO Instructions
getInstructions = do
    (cmd, flags) <- getArguments
    config <- getConfiguration cmd flags
    combineToInstructions cmd flags config

combineToInstructions :: Command -> Flags -> Configuration -> IO Instructions
combineToInstructions CommandGather Flags Configuration = pure (DispatchGather, Settings)

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
    prefs = ParserPrefs
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
parseCommand = hsubparser $ mconcat
    [ command "gather" parseCommandGather
    ]

parseCommandGather :: ParserInfo Command
parseCommandGather = info parser modifier
  where
    parser = pure CommandGather
    modifier = fullDesc
            <> progDesc "Read a single command on the standard input."

parseFlags :: Parser Flags
parseFlags = pure Flags
