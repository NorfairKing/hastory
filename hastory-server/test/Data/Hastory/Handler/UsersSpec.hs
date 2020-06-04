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
          [Entity _ newUser] <- getUsers siPool
          userName newUser `shouldBe` userFormUserName userForm
    context "username already exists" $
      it "is a 400" $ \ServerInfo {..} ->
        forAllValid $ \userForm -> do
          Right _ <- createUser siClientEnv userForm
          Left (FailureResponse _requestF resp) <- createUser siClientEnv userForm
          responseStatusCode resp `shouldBe` status400
