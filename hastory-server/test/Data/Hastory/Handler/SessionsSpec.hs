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

import Data.Hastory.API
import Data.Hastory.Server.TestUtils

spec :: Spec
spec =
  serverSpec $
  describe "POST /sessions" $ do
    context "incorrect login" $
      it "is a 401" $ \ServerInfo {..} ->
        forAllValid $ \userForm -> do
          Right _ <- runClientM (createUserClient userForm) siClientEnv
          let incorrectPasswordForm =
                userForm {userFormPassword = userFormPassword userForm <> "badsuffix"}
          Left (FailureResponse _requestF resp) <-
            runClientM (createSessionClient incorrectPasswordForm) siClientEnv
          responseStatusCode resp `shouldBe` status401
    context "correct login" $
      it "returns a cookie" $ \ServerInfo {..} ->
        forAllValid $ \userForm -> do
          Right _ <- runClientM (createUserClient userForm) siClientEnv
          Right resp <- runClientM (createSessionClient userForm) siClientEnv
          extractJWTCookie resp `shouldSatisfy` isRight
