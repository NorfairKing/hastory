{-# LANGUAGE OverloadedStrings #-}

module Data.Hastory.ServerSpec (spec) where

import Data.Hastory.API (Token(..), appendCommand)
import Data.Hastory.Server (app, Options(..), ServerSettings(..), generateToken)
import Data.Hastory.Types

import Test.Hspec

import Control.Monad.Catch (MonadThrow)
import Data.Time (UTCTime(..), fromGregorian)
import Network.HTTP.Client (newManager, defaultManagerSettings)
import Network.HTTP.Types (status403)
import Network.Wai.Handler.Warp (testWithApplication)
import Path (Path, Abs, File, parseAbsDir, toFilePath)
import Path.IO (resolveFile, withSystemTempDir)
import Servant.Client (BaseUrl(..), ClientEnv, ClientError(FailureResponse), Scheme(Http), mkClientEnv, runClientM, responseStatusCode, responseBody)

type ServerInfo = (ClientEnv, Token, Path Abs File)

testEntry :: MonadThrow m => m Entry
testEntry = parseAbsDir "/" >>= \absDir ->
              pure $ Entry "testText" absDir utcTime "localhost" "testUser"
  where utcTime = UTCTime (fromGregorian 2020 1 1) (toEnum 0)

spec :: Spec
spec = serverSpec $ do
         describe "POST /commands/append" $ do
           context "missing token" $ do
             it "is a 403 error" $ \(clientEnv, _token, _file)-> do
               entry <- testEntry
               let req = appendCommand Nothing entry
               Left (FailureResponse _ resp) <- runClientM req clientEnv
               responseBody resp `shouldBe` "X-Token header should exist."
               responseStatusCode resp `shouldBe` status403
           context "incorrect token" $ do
             it "is a 403 error" $ \(clientEnv, token, _file)-> do
               entry <- testEntry
               let req            = appendCommand (Just incorrectToken) entry
                   incorrectToken = Token (unToken token <> "badSuffix")
               Left (FailureResponse _ resp) <- runClientM req clientEnv
               responseBody resp `shouldBe` "Invalid Token provided."
               responseStatusCode resp `shouldBe` status403
           context "with correct token" $ do
             it "saves entry to csv file" $ \(clientEnv, token, file)-> do
               entry <- testEntry
               let req = appendCommand (Just token) entry
               Right () <- runClientM req clientEnv
               1 `shouldBe` 1

serverSpec :: SpecWith ServerInfo -> Spec
serverSpec = around withTestServer

withTestServer :: (ServerInfo -> IO a) -> IO a
withTestServer func = do
  manager <- newManager defaultManagerSettings
  token   <- generateToken
  withSystemTempDir "hastory-server-test" $ \tmpDir -> do
    csvFile <- resolveFile tmpDir "hastory-server.csv"
    let mkApp = pure $ app opts settings
        opts = Options 10 (toFilePath csvFile) Nothing
        settings = ServerSettings token
    testWithApplication mkApp $ \p -> do
      let clientEnv = mkClientEnv manager (BaseUrl Http "127.0.0.1" p "")
        in func (clientEnv, token, csvFile)
