{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Data.Hastory.Handler.EntriesSpec
  ( spec
  ) where

import Control.Monad
import Servant.API
import Servant.Auth.Client
import Servant.Client
import Test.Hspec
import Test.QuickCheck
import Test.Validity

import Data.Hastory.API
import Data.Hastory.Gen ()
import Data.Hastory.Server.TestUtils
import Data.Hastory.Server.Utils

spec :: Spec
spec =
  serverSpec $
  describe "POST /entries" $ do
    context "incorrect token" $
      it "is a 401" $ \ServerInfo {..} ->
        forAllValid $ \syncReq -> do
          let incorrectToken = Token "badToken"
          Left (FailureResponse _requestF resp) <-
            runClientM (createEntryClient incorrectToken syncReq) siClientEnv
          responseStatusCode resp `shouldBe` status401
    context "correct token" $ do
      it "saves entry to database" $ \ServerInfo {..} ->
        forAllValid $ \syncReq -> do
          userForm <- generate genValid
          withNewUser siClientEnv userForm $ \(userId, token) -> do
            Right _ <- runClientM (createEntryClient token syncReq) siClientEnv
            entries <- runSqlPool (selectList [] []) siPool :: IO [Entity ServerEntry]
            map entityVal entries `shouldBe` toServerEntries syncReq userId
      it "returns no content" $ \ServerInfo {..} ->
        forAllValid $ \(firstEntry, secondEntry) -> do
          userForm <- generate genValid
          withNewUser siClientEnv userForm $ \(_, token) -> do
            let syncReq = SyncRequest [firstEntry, secondEntry] "host"
            Right res <- runClientM (createEntryClient token syncReq) siClientEnv
            res `shouldBe` NoContent
      context "when same entry is sync'd twice" $ do
        it "the db does not change between the first sync and the second sync" $ \ServerInfo {..} ->
          forAllValid $ \syncReq -> do
            userForm <- generate genValid
            withNewUser siClientEnv userForm $ \(_, token) -> do
              Right _ <- runClientM (createEntryClient token syncReq) siClientEnv
              entriesAfterFirstSync <-
                runSqlPool (selectList [] []) siPool :: IO [Entity ServerEntry]
              Right _ <- runClientM (createEntryClient token syncReq) siClientEnv
              entriesAfterSecondSync <-
                runSqlPool (selectList [] []) siPool :: IO [Entity ServerEntry]
              entriesAfterSecondSync `shouldBe` entriesAfterFirstSync
        it "db only persists one entry" $ \ServerInfo {..} ->
          forAllValid $ \entry -> do
            userForm <- generate genValid
            withNewUser siClientEnv userForm $ \(_, token) -> do
              let syncReq = SyncRequest [entry] "hostname"
              replicateM_ 2 $ runClientM (createEntryClient token syncReq) siClientEnv
              dbEntries <- runSqlPool (selectList [] []) siPool :: IO [Entity ServerEntry]
              length dbEntries `shouldBe` 1
