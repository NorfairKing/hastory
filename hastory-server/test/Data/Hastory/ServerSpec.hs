{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Data.Hastory.ServerSpec
  ( spec
  ) where

import Database.Persist.Sql as SQL
import Network.HTTP.Types (status400)
import Servant.Client (ClientError (FailureResponse), responseStatusCode, runClientM)
import Test.Hspec

import Data.Hastory.API (createUserClient)
import Data.Hastory.Gen ()
import Data.Hastory.Server.TestUtils (ServerInfo (..), serverSpec)
import Data.Hastory.Types (User (userName), mkUserForm, mkUsername)

spec :: Spec
spec =
  serverSpec $ do
  describe "POST /users" $ do
    context "valid user" $
      it "creates the user" $ \ServerInfo{..} -> do
        let req = createUserClient (mkUserForm "Steven" "Passw0rd")
        _ <- runClientM req siClientEnv
        [dbUser] <- fmap SQL.entityVal <$> SQL.runSqlPool (SQL.selectList [] []) siPool
        userName dbUser `shouldBe` mkUsername "Steven"
    context "email address is not unique" $
      it "is a 400" $ \ServerInfo{..} -> do
        let req = createUserClient (mkUserForm "Paul" "SecretPassword")
        Right _ <- runClientM req siClientEnv
        Left (FailureResponse _requestF resp) <- runClientM req siClientEnv
        responseStatusCode resp `shouldBe` status400
  describe "POST /sessions" $ do
    context "incorrect login" $
      it "is a 401" (const pending)
    context "correct login" $ do
      it "is a 200" (const pending)
      it "includes the JWT header" (const pending)
  describe "POST /entries" $ do
    context "no Authentication header" $
      it "is a 401" (const pending)
    context "incorrect Authentication header" $
      it "is a 401" (const pending)
    context "correct JWT authentication" $ do
      it "saves entry to database" (const pending)
      context "when same entry is sync'd twice" $ do
        it "the db does not change between the first sync and the second sync" (const pending)
        it "db only persists one entry" (const pending)
