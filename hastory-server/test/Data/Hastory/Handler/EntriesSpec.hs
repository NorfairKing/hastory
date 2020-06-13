{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Data.Hastory.Handler.EntriesSpec
  ( spec
  ) where

import Control.Monad
import Servant.Auth.Client
import Servant.Client
import Test.Hspec
import Test.QuickCheck
import Test.Validity

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
          Left (FailureResponse _requestF resp) <- createEntry siClientEnv incorrectToken syncReq
          responseStatusCode resp `shouldBe` status401
    context "correct token" $ do
      it "saves entry to database" $ \ServerInfo {..} ->
        forAllValid $ \syncReq -> do
          userForm <- generate genValid
          withNewUser siClientEnv userForm $ \(userId, token) -> do
            Right _ <- createEntry siClientEnv token syncReq
            [Entity _ entry] <- getEntries siPool
            entry `shouldBe` toServerEntry syncReq userId
      context "when same entry is sync'd twice" $ do
        it "the db does not change between the first sync and the second sync" $ \ServerInfo {..} ->
          forAllValid $ \syncReq -> do
            userForm <- generate genValid
            withNewUser siClientEnv userForm $ \(_, token) -> do
              Right _ <- createEntry siClientEnv token syncReq
              entriesAfterFirstSync <- getEntries siPool
              Right _ <- createEntry siClientEnv token syncReq
              entriesAfterSecondSync <- getEntries siPool
              entriesAfterSecondSync `shouldBe` entriesAfterFirstSync
        it "db only persists one entry" $ \ServerInfo {..} ->
          forAllValid $ \syncReq -> do
            userForm <- generate genValid
            withNewUser siClientEnv userForm $ \(_, token) -> do
              replicateM_ 2 (createEntry siClientEnv token syncReq)
              dbEntries <- getEntries siPool
              length dbEntries `shouldBe` 1
