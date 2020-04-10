{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE RecordWildCards   #-}

module Data.Hastory.ServerSpec
  ( spec
  ) where


import qualified Database.Persist.Sqlite as SQL
import Network.HTTP.Types (status403)
import Path (absdir)
import Servant.Client (ClientError (FailureResponse), responseBody, responseStatusCode, runClientM)
import Test.Hspec
import Test.Validity (forAllValid)

import Data.Hastory.API (Token (..), appendCommand)
import Data.Hastory.Server.TestUtils (ServerInfo (..), serverSpec)
import Data.Hastory.Types
import Data.Time (UTCTime (..), fromGregorian)

spec :: Spec
spec =
  serverSpec $
  describe "POST /commands/append" $ do
    context "incorrect token" $
      it "is a 403 error with invalid token msg" $ \ServerInfo {..} -> do
        let testEntry      = Entry "testText" [absdir|/|] utcTime "localhost" "testUser"
            utcTime        = UTCTime (fromGregorian 2020 1 1) (toEnum 0)
            req            = appendCommand incorrectToken testEntry
            incorrectToken = Token (unToken siToken <> "badSuffix")
        Left (FailureResponse _ resp) <- runClientM req siClientEnv
        responseBody resp `shouldBe` "Invalid Token provided."
        responseStatusCode resp `shouldBe` status403
    context "with correct token" $
      it "saves entry to database" $ \ServerInfo {..} ->
        forAllValid $ \entry -> do
            _ <- runClientM (appendCommand siToken entry) siClientEnv
            let selectEntries = fmap SQL.entityVal <$> SQL.selectList [] []
            dbEntries <- SQL.runSqlPool selectEntries siPool
            length dbEntries `shouldBe` 1
            head dbEntries `shouldBe` entry
