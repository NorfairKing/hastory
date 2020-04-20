{-# OPTIONS_GHC -Wno-orphans #-}

module Data.Hastory.Gen where

import Data.GenValidity
import Data.Hastory
import Test.QuickCheck

instance GenValid Entry where
  genValid = genValidStructurally
  shrinkValid = shrinkValidStructurally

instance Arbitrary Entry where
  arbitrary = genValid
  shrink = shrinkValidStructurally

instance GenValid SyncRequest where
  genValid = genValidStructurally
  shrinkValid = shrinkValidStructurally
