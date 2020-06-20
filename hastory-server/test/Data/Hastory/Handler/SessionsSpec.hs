{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Data.Hastory.Handler.SessionsSpec
  ( spec
  ) where

import Data.Either
import Data.Hastory.Gen ()
import Servant.Client
import Test.Hspec
import Test.Validity

import Data.Hastory.Server.TestUtils

spec :: Spec
spec =
  serverSpec $
  describe "POST /sessions" $ do
    context "incorrect login" $
      it "is a 401" $ \ServerInfo {..} ->
        forAllValid $ \(username, password) -> do
          let userForm = UserForm username password
          Right _ <- createUser siClientEnv userForm
          let incorrectPasswordForm = UserForm username (password <> "badsuffix")
          Left (FailureResponse _requestF resp) <- loginUser siClientEnv incorrectPasswordForm
          responseStatusCode resp `shouldBe` status401
    context "correct login" $
      it "returns a cookie" $ \ServerInfo {..} -> do
        let userForm = UserForm (Username "Paul") "Passw0rd"
        Right _ <- createUser siClientEnv userForm
        Right resp <- loginUser siClientEnv userForm
        extractJWTCookie resp `shouldSatisfy` isRight
