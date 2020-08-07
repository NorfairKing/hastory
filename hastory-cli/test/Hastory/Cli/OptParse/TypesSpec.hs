{-# LANGUAGE OverloadedStrings #-}

module Hastory.Cli.OptParse.TypesSpec
  ( spec
  ) where

import Data.Aeson (Result(..), fromJSON)
import Data.Hastory
import Data.Yaml (decodeThrow)
import Servant.Client
import TestImport hiding (Failure, Result, Success)

spec :: Spec
spec =
  describe "YamlSchema Configuration" $ do
    it "is an 'partial' Configuration when user provides SOME fields" $ do
      value <- Data.Yaml.decodeThrow "username: steven"
      fromJSON value `shouldBe`
        Success (emptyConfiguration {configStorageUsername = Just (Username "steven")})
    it "is a 'full' Configuration when user provides ALL fields" $ do
      value <-
        Data.Yaml.decodeThrow
          "username: steven\npassword: Passw0rd\nurl: api.example.com\ncache-dir: ~/home"
      url <- parseBaseUrl "api.example.com"
      fromJSON value `shouldBe`
        Success
          (emptyConfiguration
             { configCacheDir = Just "~/home"
             , configStorageServer = Just url
             , configStorageUsername = Just (Username "steven")
             , configStoragePassword = Just "Passw0rd"
             })
    it "is an error when user provides the wrong type for a field" $ do
      value <- Data.Yaml.decodeThrow "url: 1"
      (fromJSON value :: Result Configuration) `shouldBe`
        Error "expected String, but encountered Number"

emptyConfiguration :: Configuration
emptyConfiguration =
  Configuration
    { configCacheDir = Nothing
    , configStorageServer = Nothing
    , configStorageUsername = Nothing
    , configStoragePassword = Nothing
    }
