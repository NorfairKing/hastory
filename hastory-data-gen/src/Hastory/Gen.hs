{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Hastory.Gen where

import Control.Applicative
import Data.GenValidity
import Data.GenValidity.Text
import qualified Data.Text as T
import Database.Persist.Sql (fromSqlKey, toSqlKey)
import Hastory.Data
import Hastory.Data.Client.DB
import Hastory.Data.Server.DB
import Test.QuickCheck

instance GenValid Entry where
  genValid = genValidStructurally
  shrinkValid = shrinkValidStructurally

instance Arbitrary Entry where
  arbitrary = genValid
  shrink = shrinkValidStructurally

instance GenValid Password where
  genValid = mkPassword <$> genValid
  shrinkValid _ = [] -- don't shrink Password

instance GenValid ServerEntryId where
  genValid = toSqlKey <$> genValid
  shrinkValid = map toSqlKey . shrinkValid . fromSqlKey

instance GenValid SyncRequest where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurally

instance GenValid UserForm where
  genValid = genValidStructurally
  shrinkValid = shrinkValidStructurally

instance GenValid Username where
  genValid = Username <$> userNameTextGen
    where
      userNameTextGen = liftA2 (<>) lengthThreeText arbitraryLengthText
      lengthThreeText = T.pack <$> vectorOf 3 asciiLetterOrDigitGen
      arbitraryLengthText = genTextBy asciiLetterOrDigitGen
      asciiLetterOrDigitGen = oneof [asciiUppercaseGen, asciiLowercaseGen, asciiDigitGen]
      asciiDigitGen = choose ('0', '9')
      asciiUppercaseGen = choose ('A', 'Z')
      asciiLowercaseGen = choose ('a', 'z')
  shrinkValid = shrinkValidStructurally
