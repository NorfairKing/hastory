{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Data.Hastory.Handler.EntriesSpec
  ( spec
  ) where

import Control.Monad
import Servant.Auth.Client
import Servant.Client
import Test.Hspec
import Test.Validity

import Data.Hastory.API
import Data.Hastory.Gen ()
import Data.Hastory.Server.TestUtils
import Data.Hastory.Server.Utils

spec :: Spec
spec = serverSpec postEntries

postEntries :: SpecWith ServerInfo
postEntries =
  describe "POST /entries" $ do
    context "incorrect token" $
      it "is a 401" $ \ServerInfo {..} ->
        forAllValid $ \syncReq -> do
          let incorrectToken = Token "badToken"
              logPosition = toSqlKey 0
          Left (FailureResponse _requestF resp) <-
            runClientM (createEntryClient incorrectToken syncReq logPosition) siClientEnv
          responseStatusCode resp `shouldBe` status401
    context "correct token" $ do
      it "saves entry to database" $ \ServerInfo {..} ->
        forAllValid $ \syncReq ->
          forAllValid $ \userForm ->
            withNewUser siClientEnv userForm $ \(userId, token) -> do
              let logPosition = toSqlKey 0
              Right _ <- runClientM (createEntryClient token syncReq logPosition) siClientEnv
              entries <- runSqlPool (selectList [] []) siPool :: IO [Entity ServerEntry]
              map entityVal entries `shouldBe` toServerEntries syncReq userId
      context "when same entry is sync'd twice" $ do
        it "the db does not change between the first sync and the second sync" $ \ServerInfo {..} ->
          forAllValid $ \syncReq ->
            forAllValid $ \userForm ->
              withNewUser siClientEnv userForm $ \(_, token) -> do
                let logPosition = toSqlKey 0
                Right _ <- runClientM (createEntryClient token syncReq logPosition) siClientEnv
                entriesAfterFirstSync <-
                  runSqlPool (selectList [] []) siPool :: IO [Entity ServerEntry]
                Right _ <- runClientM (createEntryClient token syncReq logPosition) siClientEnv
                entriesAfterSecondSync <-
                  runSqlPool (selectList [] []) siPool :: IO [Entity ServerEntry]
                entriesAfterSecondSync `shouldBe` entriesAfterFirstSync
        it "db only persists one entry" $ \ServerInfo {..} ->
          forAllValid $ \entry ->
            forAllValid $ \userForm ->
              withNewUser siClientEnv userForm $ \(_, token) -> do
                let syncReq = SyncRequest [entry] "hostname"
                    logPosition = toSqlKey 0
                replicateM_ 2 $ runClientM (createEntryClient token syncReq logPosition) siClientEnv
                dbEntries <- runSqlPool (selectList [] []) siPool :: IO [Entity ServerEntry]
                length dbEntries `shouldBe` 1
