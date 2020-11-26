{-# LANGUAGE OverloadedStrings #-}

module Hastory.OptParseSpec
  ( spec,
  )
where

import Data.Yaml as Yaml
import qualified Env
import Hastory.Server.OptParse
import Options.Applicative
import Test.Hspec

spec :: Spec
spec = do
  describe "Arguments"
    $ it "parses 'serve --port 80 --log hastory.log --key jwk.key' correctly"
    $ do
      let args = ["serve", "--port", "80", "--log", "hastory.log", "--key", "jwk.key"]
      case execParserPure prefs_ argParser args of
        CompletionInvoked _ -> expectationFailure "Completion invoked"
        Failure err -> expectationFailure $ unlines ["Failed to parse arguments: ", show err]
        Success a ->
          a
            `shouldBe` Arguments
              ( CommandServe
                  ( ServeArgs
                      { serveArgsPort = Just 80,
                        serveArgsLogFile = Just "hastory.log",
                        serveArgsKeyFile = Just "jwk.key"
                      }
                  )
              )
              (Flags {flagsConfigFile = Nothing})
  describe "Environment"
    $ it "parses HASTORY_SERVER_PORT, HASTORY_SERVER_LOG, and HASTORY_SERVER_KEY correctly"
    $ do
      let env = [("HASTORY_SERVER_PORT", "80"), ("HASTORY_SERVER_LOG", "hastory.log"), ("HASTORY_SERVER_KEY", "jwk.key")]
      case Env.parsePure environmentParser env of
        Left err -> expectationFailure $ unlines ["Failed to parse environment variables: ", show err]
        Right e ->
          e
            `shouldBe` ( Environment
                           { envPort = Just 80,
                             envLogFile = Just "hastory.log",
                             envKeyFile = Just "jwk.key",
                             envConfigFile = Nothing
                           }
                       )
  describe "Configuration"
    $ it "parses 'port', 'log', and 'key' correctly"
    $ do
      let config = object [("port", Number 80), ("log", "hastory.log"), ("key", "jwk.key")]
      case parseEither parseJSON config of
        Left err -> expectationFailure $ unlines ["Failed to parse configuration: ", show err]
        Right c ->
          c
            `shouldBe` ( Configuration
                           { configPort = Just 80,
                             configLogFile = Just "hastory.log",
                             configKeyFile = Just "jwk.key"
                           }
                       )
