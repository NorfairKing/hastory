{-# LANGUAGE OverloadedStrings #-}

module Data.Hastory.ServerSpec
  ( spec
  ) where

import Test.Hspec

import Data.Hastory.Gen ()
import Data.Hastory.Server.TestUtils (serverSpec)

spec :: Spec
spec =
  serverSpec $ do
  describe "POST /users" $ do
    context "email address is not unique" $
      it "is a 422" (const pending)
    context "valid user" $
      it "is a 201" (const pending)
  describe "POST /sessions" $ do
    context "incorrect login" $
      it "is a 401" (const pending)
    context "correct login" $ do
      it "is a 200" (const pending)
      it "includes the JWT header" (const pending)
  describe "POST /entries" $ do
    context "no Authentication header" $
      it "is a 401" (const pending)
    context "incorrect Authentication header" $
      it "is a 401" (const pending)
    context "correct JWT authentication" $ do
      it "saves entry to database" (const pending)
      context "when same entry is sync'd twice" $ do
        it "the db does not change between the first sync and the second sync" (const pending)
        it "db only persists one entry" (const pending)
