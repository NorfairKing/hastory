{-# LANGUAGE TypeApplications #-}

module HastorySpec
    ( spec
    ) where

import TestIntroduction

import Hastory.Gen ()
import Hastory.Types

spec :: Spec
spec = do
    describe "Entry" $ do
        eqSpec @Entry
        jsonSpecOnValid @Entry
        arbitrarySpec @Entry
        genValiditySpec @Entry
