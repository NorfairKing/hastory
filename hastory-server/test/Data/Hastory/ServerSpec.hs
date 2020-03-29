{-# LANGUAGE OverloadedStrings #-}

module Data.Hastory.ServerSpec
  ( spec
  ) where

import Data.Hastory.API (Token(..), appendCommand)
import Data.Hastory.Server (Options(..), ServerSettings(..), app, generateToken, prepareDb)
import Data.Hastory.Types

import Test.Hspec

import Control.Monad.Catch (MonadThrow)
import Control.Monad.Logger (runNoLoggingT)
import Data.Pool (Pool)
import Data.Time (UTCTime(..), fromGregorian)
import qualified Database.Persist.Sqlite as SQL
import Database.Persist.Sqlite (SqlBackend)
import Network.HTTP.Client (defaultManagerSettings, newManager)
import Network.HTTP.Types (status403)
import Network.Wai.Handler.Warp (testWithApplication)
import Path (parseAbsDir)
import Path.IO (resolveFile, withSystemTempDir)
import Servant.Client
  ( BaseUrl(..)
  , ClientEnv
  , ClientError(FailureResponse)
  , Scheme(Http)
  , mkClientEnv
  , responseBody
  , responseStatusCode
  , runClientM
  )

type ServerInfo = (ClientEnv, Token, Pool SqlBackend)

testEntry :: MonadThrow m => m Entry
testEntry =
  parseAbsDir "/" >>= \absDir -> pure $ Entry "testText" absDir utcTime "localhost" "testUser"
  where
    utcTime = UTCTime (fromGregorian 2020 1 1) (toEnum 0)

spec :: Spec
spec =
  serverSpec $
  describe "POST /commands/append" $ do
    context "missing token" $
      it "is a 403 error with missing header msg" $ \(clientEnv, _token, _dbPool) -> do
        entry <- testEntry
        let req = appendCommand Nothing entry
        Left (FailureResponse _ resp) <- runClientM req clientEnv
        responseBody resp `shouldBe` "X-Token header should exist."
        responseStatusCode resp `shouldBe` status403
    context "incorrect token" $
      it "is a 403 error with invalid token msg" $ \(clientEnv, token, _dbPool) -> do
        entry <- testEntry
        let req = appendCommand (Just incorrectToken) entry
            incorrectToken = Token (unToken token <> "badSuffix")
        Left (FailureResponse _ resp) <- runClientM req clientEnv
        responseBody resp `shouldBe` "Invalid Token provided."
        responseStatusCode resp `shouldBe` status403
    context "with correct token" $
      it "saves entry to database" $ \(clientEnv, token, dbPool) -> do
        entry <- testEntry
        _ <- runClientM (appendCommand (Just token) entry) clientEnv
        let selectEntries = fmap SQL.entityVal <$> SQL.selectList [] []
        dbEntries <- SQL.runSqlPool selectEntries dbPool
        length dbEntries `shouldBe` 1
        head dbEntries `shouldBe` entry

serverSpec :: SpecWith ServerInfo -> Spec
serverSpec = around withTestServer

withTestServer :: (ServerInfo -> IO a) -> IO a
withTestServer func = do
  manager <- newManager defaultManagerSettings
  token <- generateToken
  withSystemTempDir "hastory-server-test" $ \tmpDir -> do
    filePath <- resolveFile tmpDir "server.db"
    dbPool <- runNoLoggingT $ prepareDb filePath
    let mkApp = pure $ app opts settings
        opts = Options 10 Nothing
        settings = ServerSettings token dbPool
    testWithApplication mkApp $ \p ->
      let clientEnv = mkClientEnv manager (BaseUrl Http "127.0.0.1" p "")
       in func (clientEnv, token, dbPool)
