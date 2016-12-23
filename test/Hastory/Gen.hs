module Hastory.Gen where

import           TestIntroduction

import           Hastory.Types

instance Arbitrary Entry where
    arbitrary = Entry <$> arbitrary <*> arbitrary
