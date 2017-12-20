{-# LANGUAGE TypeApplications #-}

module HastorySpec
    ( spec
    ) where

import TestImport

import qualified Data.Text as T
import Safe (tailSafe)

import Hastory.Gen ()
import Hastory.SuggestAlias
import Hastory.Types

spec :: Spec
spec = do
    describe "Entry" $ do
        eqSpec @Entry
        jsonSpecOnValid @Entry
        arbitrarySpec @Entry
        genValiditySpec @Entry
    describe "commandPrefixes" $ do
        it "Lists only prefixes of the command, ignoring whitespace" $
            forAll genValid $ \e ->
            map T.concat (commandPrefixes e) `shouldSatisfy`
            all (`T.isPrefixOf` T.concat (T.words $ entryText e))
        it "Contains only nonempty prefixes" $ forAll genValid $ \e ->
            commandPrefixes e `shouldSatisfy`
            all (\p -> not $ null p || any T.null p)
    describe "aggregateSuggestions" $ do
        it "Counts each command exactly once" $ forAll genValid $ \es ->
            sum (map (round . snd) (aggregateSuggestions es)) `shouldBe`
            length es
        it "Is sorted by nonincreasing frequency" $ forAll genValid $ \es ->
            map snd (aggregateSuggestions es) `shouldSatisfy` and .
            (zipWith (>=) <*> tailSafe)
