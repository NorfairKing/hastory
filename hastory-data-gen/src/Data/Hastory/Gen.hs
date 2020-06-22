{-# OPTIONS_GHC -Wno-orphans #-}

module Data.Hastory.Gen where

import Data.GenValidity
import qualified Data.Text as T
import Test.QuickCheck

import Data.Hastory

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
  genValid = genValidStructurally
  shrinkValid = shrinkValidStructurally

instance GenValid Username where
  genValid = Username <$> validUsernameText
    where
      validUsernameText = T.pack . map unUsernameChar <$> vectorOf 4 genValid
  shrinkValid = shrinkValidStructurally

instance GenValid UsernameChar where
  genValid = UsernameChar <$> choose ('\0', '\127') `suchThat` isValid
  shrinkValid = shrinkValidStructurally
