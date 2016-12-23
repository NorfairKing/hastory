module HastorySpec
    ( spec
    ) where

import TestIntroduction

import Hastory.Gen ()
import Hastory.Types

spec :: Spec
spec = do
    describe "Entry" $ do
        eqSpec (Proxy :: Proxy Entry)
        jsonSpec (Proxy :: Proxy Entry)
        arbitrarySpec (Proxy :: Proxy Entry)
        genValiditySpec (Proxy :: Proxy Entry)
