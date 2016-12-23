module HastorySpec (spec) where

import           TestIntroduction

import           Hastory
import           Hastory.Gen
import           Hastory.Types

spec :: Spec
spec = do
    describe "Entry" $ do
        jsonSpecOnArbitrary (Proxy :: Proxy Entry)

