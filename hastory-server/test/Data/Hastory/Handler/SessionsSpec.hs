{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Data.Hastory.Handler.SessionsSpec
  ( spec
  ) where

import Data.Either
import Servant.Client
import Test.Hspec

import Data.Hastory.Server.TestUtils

spec :: Spec
spec =
  serverSpec $
  describe "POST /sessions" $ do
    context "incorrect login" $
      it "is a 401" $ \ServerInfo {..} -> do
        let userForm = mkUserForm "Paul" "Passw0rd"
        Right _ <- createUser siClientEnv userForm
        let incorrectPasswordForm = mkUserForm "Paul" "baddPassw0rd"
        Left (FailureResponse _requestF resp) <- loginUser siClientEnv incorrectPasswordForm
        responseStatusCode resp `shouldBe` status401
    context "correct login" $
      it "returns a cookie" $ \ServerInfo {..} -> do
        let userForm = mkUserForm "Paul" "Passw0rd"
        Right _ <- createUser siClientEnv userForm
        Right resp <- loginUser siClientEnv userForm
        extractJWTCookie resp `shouldSatisfy` isRight
