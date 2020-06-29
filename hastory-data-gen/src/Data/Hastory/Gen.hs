{-# OPTIONS_GHC -Wno-orphans #-}

module Data.Hastory.Gen where

import Data.GenValidity
import Data.GenValidity.Text
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
  genValid = Username <$> userNameTextGen
    where
      userNameTextGen = oneof [lengthFourText, arbitraryLengthText]
      lengthFourText = T.pack <$> vectorOf 4 asciiLetterOrDigitGen
      arbitraryLengthText = genTextBy asciiLetterOrDigitGen
      asciiLetterOrDigitGen = oneof [asciiUppercaseGen, asciiLowercaseGen, asciiDigitGen]
      asciiDigitGen = choose ('0', '9')
      asciiUppercaseGen = choose ('A', 'Z')
      asciiLowercaseGen = choose ('a', 'z')
  shrinkValid = shrinkValidStructurally
