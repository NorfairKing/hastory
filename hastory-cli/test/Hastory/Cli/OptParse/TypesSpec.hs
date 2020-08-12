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
          "username: steven\npassword: Passw0rd\nurl: api.example.com\ncache-dir: ~/home\nlrd-bypass-cache: bypass-cache"
      url <- parseBaseUrl "api.example.com"
      fromJSON value `shouldBe`
        Success
          (emptyConfiguration
             { configCacheDir = Just "~/home"
             , configStorageServer = Just url
             , configStorageUsername = Just (Username "steven")
             , configStoragePassword = Just "Passw0rd"
             , configLrdBypassCache = Just True
             })
    it "is an error when user provides the wrong type for a field" $ do
      value <- Data.Yaml.decodeThrow "url: 1"
      (fromJSON value :: Result Configuration) `shouldBe`
        Error "expected String, but encountered Number"
    context "lrd-bypass-cache" $ do
      it "parses correctly when file contains 'bypass-cache'" $ do
        value <- Data.Yaml.decodeThrow "lrd-bypass-cache: bypass-cache"
        fromJSON value `shouldBe` Success (emptyConfiguration {configLrdBypassCache = Just True})
      it "parses correctly when file contains 'no-bypass-cache'" $ do
        value <- Data.Yaml.decodeThrow "lrd-bypass-cache: no-bypass-cache"
        fromJSON value `shouldBe` Success (emptyConfiguration {configLrdBypassCache = Just False})
      it "parses correctly when file does not contain 'lrd-bypass-cache key'" $ do
        value <- Data.Yaml.decodeThrow "password: Passw0rd"
        fromJSON value `shouldBe`
          Success
            (emptyConfiguration
               {configStoragePassword = Just "Passw0rd", configLrdBypassCache = Nothing})
      it "is an error if key exists but any other value is provied" $ do
        value <- Data.Yaml.decodeThrow "lrd-bypass-cache: invalid"
        (fromJSON value :: Result Configuration) `shouldBe`
          Error "Parsing of Just \"invalid\" failed with error: Unable to parse lrd-bypass-cache."

emptyConfiguration :: Configuration
emptyConfiguration =
  Configuration
    { configCacheDir = Nothing
    , configStorageServer = Nothing
    , configStorageUsername = Nothing
    , configStoragePassword = Nothing
    , configLrdBypassCache = Nothing
    }
