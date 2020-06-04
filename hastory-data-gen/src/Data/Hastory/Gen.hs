{-# OPTIONS_GHC -Wno-orphans #-}

module Data.Hastory.Gen where

import qualified Data.Text        as T
import           Test.QuickCheck

import           Data.GenValidity
import           Data.Hastory

instance GenValid Entry where
  genValid = genValidStructurally
  shrinkValid = shrinkValidStructurally

instance Arbitrary Entry where
  arbitrary = genValid
  shrink = shrinkValidStructurally

instance GenValid Password where
  genValid = mkPassword <$> genValid
  shrinkValid _ = [] -- don't shrink Password

instance GenValid SyncRequest where
  genValid = genValidStructurally
  shrinkValid = shrinkValidStructurally

instance GenValid UserForm where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

instance GenValid Username where
  genValid = mkUsername <$> (genValid `suchThat` notNull)
    where notNull = not . T.null
  shrinkValid = map mkUsername . filter (not . T.null) . shrinkValid . rawUserName
