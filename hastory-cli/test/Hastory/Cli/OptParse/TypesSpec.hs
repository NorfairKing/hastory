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
      (fromJSON value :: Result Configuration) `shouldSatisfy` isConfigParserError
    context "cache-dir" $ do
      it "parses correctly when cache-dir key is not provided" $ do
        value <- Data.Yaml.decodeThrow "url: api.example.com"
        let Success config = fromJSON value
        configCacheDir config `shouldBe` Nothing
      it "parses correctly when cache-dir key is provided and value is valid" $ do
        value <- Data.Yaml.decodeThrow "cache-dir: ~/home"
        let Success config = fromJSON value
        configCacheDir config `shouldBe` Just "~/home"
      it "is an error when cache-dir key is provided but value is invalid" $ do
        value <- Data.Yaml.decodeThrow "cache-dir: "
        (fromJSON value :: Result Configuration) `shouldSatisfy` isConfigParserError
    context "url" $ do
      it "parses correctly when url key is not provided" $ do
        value <- Data.Yaml.decodeThrow "password: Passw0rd"
        let Success config = fromJSON value
        configStorageServer config `shouldBe` Nothing
      it "parses correctly when url key is provided" $ do
        value <- Data.Yaml.decodeThrow "url: api.example.com"
        let Success config = fromJSON value
        url <- parseBaseUrl "api.example.com"
        configStorageServer config `shouldBe` Just url
      it "is an error when url key is provided and value is invalid" $ do
        value <- Data.Yaml.decodeThrow "url: ftp://1"
        (fromJSON value :: Result Configuration) `shouldSatisfy` isConfigParserError
    context "username" $ do
      it "parses correctly when username key is not provided" $ do
        value <- Data.Yaml.decodeThrow "password: Passw0rd"
        let Success config = fromJSON value
        configStorageUsername config `shouldBe` Nothing
      it "parses correctly when username key is provided and value is valid" $ do
        value <- Data.Yaml.decodeThrow "username: steven"
        let Success config = fromJSON value
        username <- parseUsername "steven"
        configStorageUsername config `shouldBe` Just username
      it "is an error when username key is provided and value is invalid" $ do
        value <- Data.Yaml.decodeThrow "username: s"
        (fromJSON value :: Result Configuration) `shouldSatisfy` isConfigParserError
    context "password" $ do
      it "parses correctly when password key is not provided" $ do
        value <- Data.Yaml.decodeThrow "url: api.example.com"
        let Success config = fromJSON value
        configStoragePassword config `shouldBe` Nothing
      it "parses correctly when password key is provided and value is valid" $ do
        value <- Data.Yaml.decodeThrow "password: Passw0rd"
        let Success config = fromJSON value
        configStoragePassword config `shouldBe` Just "Passw0rd"
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
        (fromJSON value :: Result Configuration) `shouldSatisfy` isConfigParserError

isConfigParserError :: Result a -> Bool
isConfigParserError (Error _) = True
isConfigParserError _ = False

emptyConfiguration :: Configuration
emptyConfiguration =
  Configuration
    { configCacheDir = Nothing
    , configStorageServer = Nothing
    , configStorageUsername = Nothing
    , configStoragePassword = Nothing
    , configLrdBypassCache = Nothing
    }
