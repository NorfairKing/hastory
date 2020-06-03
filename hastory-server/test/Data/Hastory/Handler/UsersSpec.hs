{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Data.Hastory.Handler.UsersSpec (spec) where

import           Servant.Client
import           Test.Hspec

import           Data.Hastory.Server.TestUtils

spec :: Spec
spec =
  serverSpec $
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
