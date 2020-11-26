{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

module Hastory.Server.OptParse where

import Control.Monad
import Data.Maybe
import qualified Data.Text as T
import Data.Yaml
import qualified Env
import GHC.Generics (Generic)
import Options.Applicative as OptParse
import qualified Options.Applicative.Help as OptParse (string)
import Path
import Path.IO
import Text.Read
import YamlParse.Applicative as YamlParse

data Instructions = Instructions Dispatch Settings deriving (Show, Eq, Generic)

newtype Dispatch = DispatchServe ServeSettings deriving (Show, Eq, Generic)

data Settings = Settings deriving (Show, Eq, Generic)

data ServeSettings
  = ServeSettings
      { serveSettingsPort :: Int,
        serveSettingsLogFile :: Path Abs File,
        serveSettingsKeyFile :: Path Abs File
      }
  deriving (Show, Eq, Generic)

data Arguments = Arguments Command Flags deriving (Show, Eq, Generic)

newtype Command = CommandServe ServeArgs deriving (Show, Eq, Generic)

data ServeArgs
  = ServeArgs
      { serveArgsPort :: Maybe Int,
        serveArgsLogFile :: Maybe FilePath,
        serveArgsKeyFile :: Maybe FilePath
      }
  deriving (Show, Eq, Generic)

newtype Flags = Flags {flagsConfigFile :: Maybe FilePath} deriving (Show, Eq, Generic)

data Environment
  = Environment
      { envPort :: Maybe Int,
        envLogFile :: Maybe FilePath,
        envKeyFile :: Maybe FilePath,
        envConfigFile :: Maybe FilePath
      }
  deriving (Show, Eq, Generic)

data Configuration
  = Configuration
      { configPort :: Maybe Int,
        configLogFile :: Maybe FilePath,
        configKeyFile :: Maybe FilePath
      }
  deriving (Show, Eq, Generic)

instance FromJSON Configuration where
  parseJSON = viaYamlSchema

instance YamlSchema Configuration where
  yamlSchema =
    objectParser
      "Configuration"
      ( Configuration
          <$> optionalField "port" "Port to start server on."
          <*> optionalField "log" "File to save logs to."
          <*> optionalField "key" "File to read / write JWK key from / to."
      )

getInstructions :: IO Instructions
getInstructions = do
  args@(Arguments _ flags) <- getArguments
  env <- getEnvironment
  config <- getConfiguration flags env
  combineToInstructions args env config

getArguments :: IO Arguments
getArguments = customExecParser prefs_ argParser

getEnvironment :: IO Environment
getEnvironment = Env.parse (Env.header "Environment") environmentParser

combineToInstructions :: Arguments -> Environment -> Maybe Configuration -> IO Instructions
combineToInstructions (Arguments cmd _flags) Environment {..} mConf = Instructions <$> dispatch <*> settings
  where
    dispatch =
      case cmd of
        CommandServe ServeArgs {..} -> do
          let serveSettingsPort = fromMaybe 8000 (serveArgsPort <|> envPort <|> mc configPort)
          serveSettingsLogFile <-
            maybe getDefaultLogFile resolveFile' (serveArgsLogFile <|> envLogFile <|> mc configLogFile)
          serveSettingsKeyFile <-
            maybe getDefaultKeyFile resolveFile' (serveArgsKeyFile <|> envKeyFile <|> mc configKeyFile)
          pure $ DispatchServe (ServeSettings {..})
    settings = pure Settings
    mc f = mConf >>= f

getDefaultKeyFile :: IO (Path Abs File)
getDefaultKeyFile = do
  xdgConfigDir <- getXdgDir XdgData (Just [reldir|hastory-server|])
  resolveFile xdgConfigDir "hastory.key"

getDefaultLogFile :: IO (Path Abs File)
getDefaultLogFile = do
  xdgConfigDir <- getXdgDir XdgData (Just [reldir|hastory-server|])
  resolveFile xdgConfigDir "hastory.logs"

getDefaultConfigFile :: IO (Path Abs File)
getDefaultConfigFile = do
  xdgConfigDir <- getXdgDir XdgConfig (Just [reldir|hastory-server|])
  resolveFile xdgConfigDir "config.yaml"

getConfiguration :: Flags -> Environment -> IO (Maybe Configuration)
getConfiguration Flags {..} Environment {..} =
  case flagsConfigFile <|> envConfigFile of
    Nothing -> getDefaultConfigFile >>= YamlParse.readConfigFile
    Just cf -> resolveFile' cf >>= YamlParse.readConfigFile

prefs_ :: OptParse.ParserPrefs
prefs_ = OptParse.defaultPrefs {OptParse.prefShowHelpOnError = True, OptParse.prefShowHelpOnEmpty = True}

argParser :: OptParse.ParserInfo Arguments
argParser =
  OptParse.info
    (OptParse.helper <*> parseArgs)
    (OptParse.fullDesc <> OptParse.footerDoc (Just $ OptParse.string footerStr))
  where
    footerStr =
      unlines
        [ Env.helpDoc environmentParser,
          "",
          "Configuration file format:",
          T.unpack (YamlParse.prettySchemaDoc @Configuration)
        ]

parseArgs :: OptParse.Parser Arguments
parseArgs = Arguments <$> parseCommand <*> parseFlags

parseCommand :: OptParse.Parser Command
parseCommand =
  OptParse.hsubparser $
    mconcat
      [ OptParse.command "serve" $ CommandServe <$> parseCommandServe
      ]

parseCommandServe :: OptParse.ParserInfo ServeArgs
parseCommandServe =
  OptParse.info
    (OptParse.helper <*> parseServeArgs)
    (OptParse.fullDesc <> OptParse.progDesc "Start the server.")

parseServeArgs :: OptParse.Parser ServeArgs
parseServeArgs = ServeArgs <$> parsePort <*> parseLogFile <*> parseKeyFile
  where
    parsePort = optional $ option auto (short 'p' <> long "port" <> metavar "PORT" <> help "The port at which the server will be available.")
    parseLogFile = optional $ strOption (short 'l' <> long "log" <> metavar "LOG" <> help "The file to which server logs will be written.")
    parseKeyFile = optional $ strOption (short 'k' <> long "key" <> metavar "KEY" <> help "The file to which server keys will be read from / written to.")

parseFlags :: OptParse.Parser Flags
parseFlags = Flags <$> configFileParser
  where
    configFileParser :: OptParse.Parser (Maybe FilePath)
    configFileParser = optional $ strOption (short 'c' <> long "config-file" <> metavar "FILEPATH" <> help "Path to an alternative config file.")

environmentParser :: Env.Parser Env.Error Environment
environmentParser =
  Env.prefixed "HASTORY_SERVER_" $
    Environment
      <$> Env.var readNum "PORT" (Env.help "The port at which the server will be available." <> Env.def Nothing)
      <*> Env.var (pure . Just <=< Env.nonempty) "LOG" (Env.help "The file to which server logs will be written." <> Env.def Nothing)
      <*> Env.var (pure . Just <=< Env.nonempty) "KEY" (Env.help "The file to which server keys will be read from / written to." <> Env.def Nothing)
      <*> Env.var (pure . Just <=< Env.nonempty) "CONFIG_FILE" (Env.help "Path to an alternative config file." <> Env.def Nothing)
  where
    readNum :: String -> Either Env.Error (Maybe Int)
    readNum s = case readMaybe s of
      Nothing -> Left $ Env.UnreadError (unwords ["Could not parse '", s, "' into a number"])
      Just i -> Right (Just i)
