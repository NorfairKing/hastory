module Hastory.Gen where

import TestIntroduction

import Hastory.Types

instance GenUnchecked Entry where
    genUnchecked = Entry <$> arbitrary <*> genUnchecked <*> arbitrary

instance GenValid Entry where
    genValid = Entry <$> genValid <*> genValid <*> arbitrary

instance Arbitrary Entry where
    arbitrary = genValid
