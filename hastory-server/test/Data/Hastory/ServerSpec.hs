{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE QuasiQuotes #-}

module Data.Hastory.ServerSpec
  ( spec
  ) where

import Data.Hastory.API (Token(..), appendCommand)
import Data.Hastory.Server (Options(..), ServerSettings(..), app, generateToken, prepareDb)
import Data.Hastory.Types

import Test.Hspec

import Control.Monad.Logger (runNoLoggingT)
import Data.Pool (Pool)
import Data.Time (UTCTime(..), fromGregorian)
import qualified Database.Persist.Sqlite as SQL
import Database.Persist.Sqlite (SqlBackend)
import Network.HTTP.Client (defaultManagerSettings, newManager)
import Network.HTTP.Types (status403)
import Network.Wai.Handler.Warp (testWithApplication)
import Path (absdir)
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

data ServerInfo =
  ServerInfo
    { siClientEnv :: ClientEnv
    , siToken :: Token
    , siPool :: Pool SqlBackend
    }

testEntry :: Entry
testEntry = Entry "testText" absDir utcTime "localhost" "testUser"
  where
    absDir = [absdir|/|]
    utcTime = UTCTime (fromGregorian 2020 1 1) (toEnum 0)

spec :: Spec
spec =
  serverSpec $
  describe "POST /commands/append" $ do
    context "incorrect token" $
      it "is a 403 error with invalid token msg" $ \ServerInfo {..} -> do
        let req = appendCommand incorrectToken testEntry
            incorrectToken = Token (unToken siToken <> "badSuffix")
        Left (FailureResponse _ resp) <- runClientM req siClientEnv
        responseBody resp `shouldBe` "Invalid Token provided."
        responseStatusCode resp `shouldBe` status403
    context "with correct token" $
      it "saves entry to database" $ \ServerInfo {..} -> do
        _ <- runClientM (appendCommand siToken testEntry) siClientEnv
        let selectEntries = fmap SQL.entityVal <$> SQL.selectList [] []
        dbEntries <- SQL.runSqlPool selectEntries siPool
        length dbEntries `shouldBe` 1
        head dbEntries `shouldBe` testEntry

serverSpec :: SpecWith ServerInfo -> Spec
serverSpec = around withTestServer

withTestServer :: (ServerInfo -> IO a) -> IO a
withTestServer func = do
  manager <- newManager defaultManagerSettings
  siToken <- generateToken
  withSystemTempDir "hastory-server-test" $ \tmpDir -> do
    filePath <- resolveFile tmpDir "server.db"
    siPool <- runNoLoggingT $ prepareDb filePath
    let mkApp = pure $ app opts settings
        opts = Options 10 Nothing
        settings = ServerSettings siToken siPool
    testWithApplication mkApp $ \p ->
      let siClientEnv = mkClientEnv manager (BaseUrl Http "127.0.0.1" p "")
       in func (ServerInfo {..})
