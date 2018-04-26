{-# OPTIONS_GHC -Wno-orphans #-}

module Data.Hastory.Gen where

import Import

import Data.Hastory

instance GenUnchecked Entry

instance GenValid Entry where
    genValid =
        (Entry <$> genValid <*> genValid <*> genValid <*> genValid <*> genValid) `suchThat`
        isValid

instance GenInvalid Entry

instance Arbitrary Entry where
    arbitrary = genValid
