{-# OPTIONS_GHC -Wno-orphans #-}

module Data.Hastory.Gen where

import Data.GenValidity
import Data.GenValidity.Text (genTextBy)
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
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

instance GenValid Username where
  genValid = Username <$> validUsernameText
    where
      validUsernameText = genTextBy asciiDigitOrLetter `suchThat` (not . T.null)
      asciiDigitOrLetter = ascii `suchThat` (validationIsValid . validUsernameChar)
      ascii = choose ('\0', '\127')
  shrinkValid = map Username . filter (not . T.null) . shrinkValid . usernameText
