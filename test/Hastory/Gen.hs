{-# OPTIONS_GHC -Wno-orphans #-}

module Hastory.Gen where

import TestImport

import Hastory.Types

instance GenUnchecked Entry where
    genUnchecked = Entry <$> genUnchecked <*> genUnchecked <*> genUnchecked <*> genUnchecked

instance GenValid Entry where
    genValid = Entry <$> genValid <*> genValid <*> genValid <*> genValid

instance GenInvalid Entry

instance Arbitrary Entry where
    arbitrary = genValid
