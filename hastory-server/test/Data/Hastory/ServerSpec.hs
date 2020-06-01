{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Data.Hastory.ServerSpec
  ( spec
  ) where

import           Control.Monad
import           Data.Maybe
import           Data.Pool
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
    context "valid new user request" $
      it "creates the user" $ \ServerInfo{..} -> do
        let userForm = mkUserForm "Steven" "Passw0rd"

        Right _ <- createUser siClientEnv userForm

        [Entity _ newUser] <- getUsers siPool

        userName newUser `shouldBe` mkUsername "Steven"
    context "username already exists" $
      it "is a 400" $ \ServerInfo{..} -> do
        let userForm = mkUserForm "Steven" "Passw0rd"

        Right _ <- createUser siClientEnv userForm

        Left (FailureResponse _requestF resp) <-
          createUser siClientEnv userForm

        responseStatusCode resp `shouldBe` status400
  describe "POST /sessions" $ do
    context "incorrect login" $
      it "is a 401" $ \ServerInfo{..} -> do
        let userForm = mkUserForm "Paul" "Passw0rd"

        Right _ <- createUser siClientEnv userForm

        let incorrectPasswordForm = mkUserForm "Paul" "baddPassw0rd"

        Left (FailureResponse _requestF resp) <-
          loginUser siClientEnv incorrectPasswordForm

        responseStatusCode resp `shouldBe` status401
    context "correct login" $
      it "returns a cookie" $ \ServerInfo{..} -> do
        let userForm = mkUserForm "Paul" "Passw0rd"

        Right _ <- createUser siClientEnv userForm

        Right resp <- loginUser siClientEnv userForm

        extractJWTCookie resp `shouldSatisfy` isJust
  describe "POST /entries" $ do
    context "incorrect token" $
      it "is a 401" $ \ServerInfo{..} ->
        forAllValid $ \syncReq -> do
          let incorrectToken = Token "badToken"

          Left (FailureResponse _requestF resp) <-
            createEntry siClientEnv incorrectToken syncReq

          responseStatusCode resp `shouldBe` status401
    context "correct token" $ do
      it "saves entry to database" $ \ServerInfo{..} ->
        forAllValid $ \syncReq ->
          withNewUser siClientEnv $ \(userId, token) -> do
            Right _ <- createEntry siClientEnv token syncReq

            [Entity _ entry] <- getEntries siPool

            entry `shouldBe` toServerEntry syncReq userId
      context "when same entry is sync'd twice" $ do
        it "the db does not change between the first sync and the second sync" $ \ServerInfo{..} ->
          forAllValid $ \syncReq ->
            withNewUser siClientEnv $ \(_, token) -> do
              Right _ <- createEntry siClientEnv token syncReq

              entriesAfterFirstSync <- getEntries siPool

              Right _ <- createEntry siClientEnv token syncReq

              entriesAfterSecondSync <- getEntries siPool

              entriesAfterSecondSync  `shouldBe` entriesAfterFirstSync
        it "db only persists one entry" $ \ServerInfo{..} ->
          forAllValid $ \syncReq ->
            withNewUser siClientEnv $ \(_, token) -> do
              replicateM_ 2 (createEntry siClientEnv token syncReq)

              dbEntries <- getEntries siPool

              length dbEntries `shouldBe` 1


createEntry :: ClientEnv -> Token -> SyncRequest -> IO (Either ClientError NoContent)
createEntry clientEnv token syncReq = runClientM (createEntryClient token syncReq) clientEnv

createUser :: ClientEnv -> UserForm -> IO (Either ClientError UserId)
createUser clientEnv userForm = runClientM (createUserClient userForm) clientEnv

getEntries :: Pool SqlBackend -> IO [Entity ServerEntry]
getEntries = runSqlPool (selectList [] [])

getUsers :: Pool SqlBackend -> IO [Entity User]
getUsers = runSqlPool (selectList [] [])

loginUser :: ClientEnv -> UserForm -> IO (Either ClientError (Headers AuthCookies NoContent))
loginUser clientEnv userForm = runClientM (createSessionClient userForm) clientEnv
