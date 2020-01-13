{-# LANGUAGE TypeApplications #-}

module Data.Hastory.InstanceSpec
  ( spec
  ) where

import TestImport

import Data.Hastory
import Data.Hastory.Gen ()

spec :: Spec
spec =
  describe "Entry" $ do
    eqSpecOnValid @Entry
    arbitrarySpec @Entry
    genValidSpec @Entry
