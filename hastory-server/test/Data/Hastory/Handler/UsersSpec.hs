{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Data.Hastory.Handler.UsersSpec
  ( spec
  ) where

import Servant.Client
import Test.Hspec
import Test.Validity

import Data.Hastory.Gen ()
import Data.Hastory.Server.TestUtils

spec :: Spec
spec =
  serverSpec $
  describe "POST /users" $ do
    context "valid new user request" $
      it "creates the user" $ \ServerInfo {..} ->
        forAllValid $ \userForm -> do
          Right _ <- createUser siClientEnv userForm
          [Entity _ newUser] <- runSqlPool (selectList [] []) siPool
          userName newUser `shouldBe` userFormUserName userForm
    context "userForm is invalid" $
      it "does not create the user" $ \ServerInfo {..} -> do
        let invalidUserName =
              "\192400\440428\904918\344036\355\177961\879579\1046203\470521\1025773"
            userForm = UserForm (Username invalidUserName) "Password"
        Left (FailureResponse _requestF resp) <- createUser siClientEnv userForm
        responseStatusCode resp `shouldBe` status400
        users <- runSqlPool (selectList [] []) siPool :: IO [Entity User]
        length users `shouldBe` 0
    context "username already exists" $
      it "is a 400" $ \ServerInfo {..} ->
        forAllValid $ \userForm -> do
          Right _ <- createUser siClientEnv userForm
          Left (FailureResponse _requestF resp) <- createUser siClientEnv userForm
          responseStatusCode resp `shouldBe` status400
