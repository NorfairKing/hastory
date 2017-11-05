{-# LANGUAGE TypeApplications #-}

module HastorySpec
    ( spec
    ) where

import TestImport

import Hastory.Gen ()
import Hastory.Types

spec :: Spec
spec =
    describe "Entry" $ do
        eqSpec @Entry
        jsonSpecOnValid @Entry
        arbitrarySpec @Entry
        genValiditySpec @Entry
