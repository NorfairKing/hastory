module Hastory.Gen where

import           TestIntroduction

import           Hastory.Types

instance GenValidity Entry where
    genUnchecked = Entry
        <$> arbitrary
        <*> genUnchecked
        <*> arbitrary

    genValid = Entry
        <$> genValid
        <*> genValid
        <*> arbitrary

instance Arbitrary Entry where
    arbitrary = genValid
