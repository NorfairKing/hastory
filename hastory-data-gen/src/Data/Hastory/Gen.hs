{-# OPTIONS_GHC -Wno-orphans #-}

module Data.Hastory.Gen where

import Data.GenValidity
import Data.GenValidity.Path ()
import Data.GenValidity.Text ()
import Data.GenValidity.Time ()
import Data.Hastory
import Test.QuickCheck

instance GenUnchecked Entry

instance GenValid Entry where
  genValid =
    (Entry <$> genValid <*> genValid <*> genValid <*> genValid <*> genValid) `suchThat`
    isValid

instance GenInvalid Entry

instance Arbitrary Entry where
  arbitrary = genValid
