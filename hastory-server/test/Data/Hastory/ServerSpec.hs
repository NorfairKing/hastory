{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Data.Hastory.ServerSpec
  ( spec
  ) where

import           Control.Monad
import           Database.Persist.Sql          as SQL
import           Network.HTTP.Types
import           Servant.API
import           Servant.Auth.Client
import           Servant.Client
import           Test.Hspec
import           Test.Validity

import           Data.Hastory.API
import           Data.Hastory.Gen              ()
import           Data.Hastory.Server.TestUtils
import           Data.Hastory.Server.Utils
import           Data.Hastory.Types

spec :: Spec
spec =
  serverSpec $ do
  describe "POST /users" $ do
    context "valid user" $
      it "the user" $ \ServerInfo{..} -> do
        let req = createUserClient (mkUserForm "Steven" "Passw0rd")
        _ <- runClientM req siClientEnv
        [dbUser] <- fmap SQL.entityVal <$> SQL.runSqlPool (SQL.selectList [] []) siPool
        userName dbUser `shouldBe` mkUsername "Steven"
    context "username already exists" $
      it "is a 400" $ \ServerInfo{..} -> do
        let req = createUserClient (mkUserForm "Paul" "SecretPassword")
        Right _ <- runClientM req siClientEnv
        Left (FailureResponse _requestF resp) <- runClientM req siClientEnv
        responseStatusCode resp `shouldBe` status400
  describe "POST /sessions" $ do
    context "incorrect login" $
      it "is a 401" $ \ServerInfo{..} -> do
        let createUserReq = createUserClient (mkUserForm "Paul" "SecretPassword")
        Right _ <- runClientM createUserReq siClientEnv

        let createSessionReq = createSessionClient (mkUserForm "Paul" "incorrect password")
        Left (FailureResponse _requestF resp) <- runClientM createSessionReq siClientEnv
        responseStatusCode resp `shouldBe` status401
    context "correct login" $
      it "returns a cookie" $ \ServerInfo{..} -> do
        let userForm = mkUserForm "Paul" "SecretPassword"
            createUserReq = createUserClient userForm
        Right _ <- runClientM createUserReq siClientEnv

        let createSessionReq = createSessionClient userForm
        Right resp <- runClientM createSessionReq siClientEnv
        let headers = getHeaders resp
        fmap fst headers `shouldContain` ["Set-Cookie"]
  describe "POST /entries" $ do
    context "incorrect token" $
      it "is a 401" $ \ServerInfo{..} ->
        forAllValid $ \syncReq -> do
          let createEntryReq = createEntryClient incorrectToken syncReq
              incorrectToken = Token "badToken"
          Left (FailureResponse _requestF resp) <- runClientM createEntryReq siClientEnv
          responseStatusCode resp `shouldBe` status401
    context "correct token" $ do
      it "saves entry to database" $ \ServerInfo{..} ->
        forAllValid $ \syncReq ->
          withNewUser siClientEnv $ \(userId, token) -> do
            let createEntryReq = createEntryClient token syncReq
            Right _ <- runClientM createEntryReq siClientEnv

            let getEntries = fmap entityVal <$> selectList [] []
            entries <- runSqlPool getEntries siPool
            entries `shouldBe` [toServerEntry syncReq userId]
      context "when same entry is sync'd twice" $ do
        it "the db does not change between the first sync and the second sync" $ \ServerInfo{..} ->
          forAllValid $ \syncReq ->
            withNewUser siClientEnv $ \(_, token) -> do
              let createEntryReq = createEntryClient token syncReq
              _ <- runClientM createEntryReq siClientEnv

              entriesAfterFirstSync <- runSqlPool (selectList [] []) siPool

              _ <- runClientM createEntryReq siClientEnv
              entriesAfterSecondSync <- runSqlPool (selectList [] []) siPool

              (entriesAfterSecondSync :: [Entity ServerEntry]) `shouldBe` entriesAfterFirstSync
        it "db only persists one entry" $ \ServerInfo{..} ->
          forAllValid $ \syncReq ->
            withNewUser siClientEnv $ \(_, token) -> do
              let createEntryReq = createEntryClient token syncReq
              replicateM_ 2 (runClientM createEntryReq siClientEnv)
              dbEntries <- runSqlPool (selectList [] []) siPool
              length (dbEntries :: [Entity ServerEntry]) `shouldBe` 1
