{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Data.Hastory.ServerSpec
  ( spec
  ) where

import Control.Monad (replicateM_)
import qualified Database.Persist.Sqlite as SQL
import Network.HTTP.Types (status403)
import Servant.Client (ClientError(FailureResponse), responseBody, responseStatusCode, runClientM)
import Test.Hspec
import Test.Validity (forAllValid)

import Data.Hastory.API (Token(..), appendCommand)
import Data.Hastory.Gen ()
import Data.Hastory.Server.TestUtils (ServerInfo(..), serverSpec)
import Data.Hastory.Server.Utils (toServerEntry)
import Data.Hastory.Types as SyncRequest

spec :: Spec
spec =
  serverSpec $
  describe "POST /commands/append" $ do
    context "incorrect token" $
      it "is a 403 error with invalid token msg" $ \ServerInfo {..} ->
        forAllValid $ \syncRequest -> do
          let req = appendCommand incorrectToken syncRequest
              incorrectToken = Token (unToken siToken <> "badSuffix")
          Left (FailureResponse _ resp) <- runClientM req siClientEnv
          responseBody resp `shouldBe` "Invalid Token provided."
          responseStatusCode resp `shouldBe` status403
    context "with correct token" $ do
      it "saves entry to database" $ \ServerInfo {..} ->
        forAllValid $ \syncRequest -> do
          _ <- runClientM (appendCommand siToken syncRequest) siClientEnv
          let selectEntries = fmap SQL.entityVal <$> SQL.selectList [] []
          dbEntries <- SQL.runSqlPool selectEntries siPool
          dbEntries `shouldBe` [toServerEntry syncRequest]
      it "is idempotent" $ \ServerInfo {..} ->
        forAllValid $ \syncRequest -> do
          replicateM_ 2 $ runClientM (appendCommand siToken syncRequest) siClientEnv
          let selectEntries = fmap SQL.entityVal <$> SQL.selectList [] []
          dbEntries <- SQL.runSqlPool selectEntries siPool
          length (dbEntries :: [ServerEntry]) `shouldBe` 1
