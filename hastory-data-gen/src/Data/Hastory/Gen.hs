{-# OPTIONS_GHC -Wno-orphans #-}

module Data.Hastory.Gen where

import Data.GenValidity
import Data.GenValidity.Path ()
import Data.GenValidity.Text ()
import Data.GenValidity.Time ()
import Data.Hastory
import Test.QuickCheck

instance GenValid Entry where
  genValid = genValidStructurally
  shrinkValid = shrinkValidStructurally

instance Arbitrary Entry where
  arbitrary = genValid
  shrink = shrinkValidStructurally
