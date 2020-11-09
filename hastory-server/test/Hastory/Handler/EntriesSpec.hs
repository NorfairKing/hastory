{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Hastory.Handler.EntriesSpec
  ( spec,
  )
where

import Control.Monad
import Hastory.API
import Hastory.Data
import Hastory.Data.Server.DB
import Hastory.Gen ()
import Hastory.Server.TestUtils
import Hastory.Server.Utils
import Servant.Auth.Client
import Servant.Client
import Test.Hspec
import Test.Validity

spec :: Spec
spec = serverSpec postEntries

postEntries :: SpecWith ServerInfo
postEntries =
  describe "POST /entries" $ do
    context "incorrect token"
      $ it "is a 401"
      $ \ServerInfo {..} ->
        forAllValid $ \syncReq -> do
          let incorrectToken = Token "badToken"
          Left (FailureResponse _requestF resp) <-
            runClientM (createEntryClient incorrectToken syncReq) siClientEnv
          responseStatusCode resp `shouldBe` status401
    context "correct token" $ do
      it "saves entry to database" $ \ServerInfo {..} ->
        forAllValid $ \syncReq ->
          forAllValid $ \userForm ->
            withNewUser siClientEnv userForm $ \(userId, token) -> do
              Right _ <- runClientM (createEntryClient token syncReq) siClientEnv
              entries <- runSqlPool (selectList [] []) siPool :: IO [Entity ServerEntry]
              map entityVal entries `shouldBe` toServerEntries syncReq userId
      context "when same entry is sync'd twice" $ do
        it "the db does not change between the first sync and the second sync" $ \ServerInfo {..} ->
          forAllValid $ \syncReq ->
            forAllValid $ \userForm ->
              withNewUser siClientEnv userForm $ \(_, token) -> do
                Right _ <- runClientM (createEntryClient token syncReq) siClientEnv
                entriesAfterFirstSync <-
                  runSqlPool (selectList [] []) siPool :: IO [Entity ServerEntry]
                Right _ <- runClientM (createEntryClient token syncReq) siClientEnv
                entriesAfterSecondSync <-
                  runSqlPool (selectList [] []) siPool :: IO [Entity ServerEntry]
                entriesAfterSecondSync `shouldBe` entriesAfterFirstSync
        it "db only persists one entry" $ \ServerInfo {..} ->
          forAllValid $ \entry ->
            forAllValid $ \userForm ->
              withNewUser siClientEnv userForm $ \(_, token) -> do
                let syncReq = SyncRequest [entry] "hostname" (toSqlKey 0)
                replicateM_ 2 $ runClientM (createEntryClient token syncReq) siClientEnv
                dbEntries <- runSqlPool (selectList [] []) siPool :: IO [Entity ServerEntry]
                length dbEntries `shouldBe` 1
        it "responds with server entries" $ \ServerInfo {..} ->
          forAllValid $ \userForm ->
            forAllValid $ \entry ->
              withNewUser siClientEnv userForm $ \(_userId, token) -> do
                let syncReq = SyncRequest [entry] "hostname" (toSqlKey 0)
                Right responseEntries <- runClientM (createEntryClient token syncReq) siClientEnv
                serverEntries <- runSqlPool (selectList [] []) siPool
                serverEntries `shouldBe` responseEntries
        it "responds with server entries that belong to the user" $ \ServerInfo {..} ->
          forAllValid $ \userFormOne ->
            forAllValid $ \userFormTwo ->
              forAllValid $ \entryOne ->
                forAllValid $ \entryTwo ->
                  withNewUser siClientEnv userFormOne $ \(userIdOne, userOneToken) ->
                    withNewUser siClientEnv userFormTwo $ \(userIdTwo, _userTwoToken) -> do
                      let serverEntryOne = toServerEntry userIdOne "host" entryOne
                          serverEntryTwo = toServerEntry userIdTwo "host" entryTwo
                      userOneServerEntry <- runSqlPool (insertEntity serverEntryOne) siPool
                      _userTwoServerEntry <- runSqlPool (insertEntity serverEntryTwo) siPool
                      let syncReq = SyncRequest [entryOne] "host" (toSqlKey 0)
                      Right responseEntries <-
                        runClientM (createEntryClient userOneToken syncReq) siClientEnv
                      responseEntries `shouldBe` [userOneServerEntry]
        it "responds with server entries that have an ID greater than the logPosition" $ \ServerInfo {..} ->
          forAllValid $ \(entryOne, entryTwo, entryThree) ->
            forAllValid $ \userForm ->
              withNewUser siClientEnv userForm $ \(userId, token) -> do
                let serverEntries =
                      map (toServerEntry userId "host") [entryOne, entryTwo, entryThree]
                [_small, mid, large] <- runSqlPool (insertMany serverEntries) siPool
                let syncReq = SyncRequest [] "host" mid
                Right responseEntries <- runClientM (createEntryClient token syncReq) siClientEnv
                map entityKey responseEntries `shouldBe` [large]
