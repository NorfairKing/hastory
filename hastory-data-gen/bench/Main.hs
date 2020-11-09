{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Criterion.Main as Criterion
import Criterion.Types as Criterion
import Data.GenValidity
import Hastory.API.Gather
import Hastory.Data.Client.DB
import Hastory.Gen ()
import Test.QuickCheck

main :: IO ()
main =
  let config = Criterion.defaultConfig {Criterion.reportFile = Just "bench.html"}
   in Criterion.defaultMainWith
        config
        [ bench "generate-valid-entry" $ nfIO $ generate (genValid :: Gen Entry),
          bench "gather" $ nfIO $ gatherEntryWith "ls -lr"
        ]
