{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Data.Hastory.Handler.UsersSpec
  ( spec
  ) where

import qualified Data.Text as T
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
    context "username already exists" $ do
      it "is a 400" $ \ServerInfo {..} ->
        forAllValid $ \userForm -> do
          Right _ <- createUser siClientEnv userForm
          Left (FailureResponse _requestF resp) <- createUser siClientEnv userForm
          responseStatusCode resp `shouldBe` status400
      it "equality for a user's name is case insensitive" $ \ServerInfo {..} ->
        forAllValid $ \userName -> do
          let formAllLowerCase = mkUserForm lowercasedUsername "password"
              lowercasedUsername = T.toLower . rawUserName $ userName
          Right _ <- createUser siClientEnv formAllLowerCase
          let formAllUpperCase = mkUserForm uppercasedUsername "password"
              uppercasedUsername = T.toUpper . rawUserName $ userName
          Left (FailureResponse _requestF resp) <- createUser siClientEnv formAllUpperCase
          responseStatusCode resp `shouldBe` status400
