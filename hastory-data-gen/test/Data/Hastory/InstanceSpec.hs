{-# LANGUAGE TypeApplications #-}

module Data.Hastory.InstanceSpec
    ( spec
    ) where

import TestImport

import Data.Hastory
import Data.Hastory.Gen ()

spec :: Spec
spec = do
    describe "Entry" $ do
        eqSpec @Entry
        arbitrarySpec @Entry
        genValiditySpec @Entry
