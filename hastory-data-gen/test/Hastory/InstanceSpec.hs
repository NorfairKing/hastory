{-# LANGUAGE TypeApplications #-}

module Hastory.InstanceSpec
  ( spec
  ) where

import TestImport

import Hastory.Data.Client.DB
import Hastory.Gen ()

spec :: Spec
spec =
  describe "Entry" $ do
    eqSpecOnValid @Entry
    arbitrarySpec @Entry
    genValidSpec @Entry
