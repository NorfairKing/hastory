{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Data.Hastory.Handler.UsersSpec
  ( spec
  ) where

import qualified Data.ByteString.Lazy.Char8 as C
import Servant.Client
import Test.Hspec
import Test.Validity

import Data.Hastory.API
import Data.Hastory.Gen ()
import Data.Hastory.Server.TestUtils

spec :: Spec
spec =
  serverSpec $
  describe "POST /users" $ do
    context "valid new user request" $
      it "creates the user" $ \ServerInfo {..} ->
        forAllValid $ \userForm -> do
          Right _ <- runClientM (createUserClient userForm) siClientEnv
          [Entity _ newUser] <- runSqlPool (selectList [] []) siPool
          userName newUser `shouldBe` userFormUserName userForm
    context "userForm is invalid" $ do
      it "does not create the user" $ \ServerInfo {..} -> do
        let invalidUserName =
              "\192400\440428\904918\344036\355\177961\879579\1046203\470521\1025773"
            userForm = UserForm (Username invalidUserName) "Password"
        Left (FailureResponse _requestF resp) <- runClientM (createUserClient userForm) siClientEnv
        responseStatusCode resp `shouldBe` status400
        users <- runSqlPool (selectList [] []) siPool :: IO [Entity User]
        length users `shouldBe` 0
      it "shows appropriate error message" $ \ServerInfo {..} -> do
        let invalidUserName = "\0"
            userForm = UserForm (Username invalidUserName) "Password"
        Left (FailureResponse _requestF resp) <- runClientM (createUserClient userForm) siClientEnv -- Either ClientError UserId
        let errMsg = C.unpack (responseBody resp)
        print errMsg
        errMsg `shouldContain` "The character is not a letter or digit"
    context "username already exists" $
      it "is a 400" $ \ServerInfo {..} ->
        forAllValid $ \userForm -> do
          Right _ <- runClientM (createUserClient userForm) siClientEnv
          Left (FailureResponse _requestF resp) <-
            runClientM (createUserClient userForm) siClientEnv
          responseStatusCode resp `shouldBe` status400
