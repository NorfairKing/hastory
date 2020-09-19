{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

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
  serverSpec $ do
    postEntries
    getEntries

postEntries :: SpecWith ServerInfo
postEntries =
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

getEntries :: SpecWith ServerInfo
getEntries =
  describe "GET /entries" $ do
    it "returns server entries greater than position" $ \ServerInfo {..} -> do
      userForm <- generate genValid
      withNewUser siClientEnv userForm $ \(userId, token) -> do
        entries <- generate $ vectorOf 3 genValid
        let serverEntries = map (toServerEntry userId "someHost") entries
        _ <- runSqlPool (insertMany serverEntries) siPool -- save to sync server
        [_small, mid, large] <- runSqlPool (selectList [] [Asc ServerEntryId]) siPool
        let midId = entityKey mid
        Right returnedEntries <- runClientM (getEntryClient token midId) siClientEnv
        returnedEntries `shouldBe` [large]
    it "only returns server entries that belong to logged-in user" $ \ServerInfo {..} -> do
      firstUserForm <- generate genValid
      withNewUser siClientEnv firstUserForm $ \(userOneId, _token) -> do
        secondUserForm <- generate genValid
        withNewUser siClientEnv secondUserForm $ \(userTwoId, userTwoToken) -> do
          [firstEntry, secondEntry] <- generate $ vectorOf 2 genValid
          let serverEntries =
                [ toServerEntry userOneId "user1Host" firstEntry
                , toServerEntry userTwoId "user2Host" secondEntry
                ]
          _ <- runSqlPool (insertMany serverEntries) siPool -- save to sync server
          entriesForUserTwo <- runSqlPool (selectList [ServerEntryUser ==. userTwoId] []) siPool
          let zero = toSqlKey 0
          Right returnedEntries <- runClientM (getEntryClient userTwoToken zero) siClientEnv
          returnedEntries `shouldBe` entriesForUserTwo
