{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import Import

import Control.Monad.Catch
import Control.Monad.Reader
import Control.Monad.State

import Data.GenValidity
import Test.QuickCheck

import Criterion.Main as Criterion
import Criterion.Types as Criterion

import Data.Hastory
import Data.Hastory.Gen ()

main :: IO ()
main =
  let config =
        Criterion.defaultConfig {Criterion.reportFile = Just "bench.html"}
   in Criterion.defaultMainWith
        config
        [ bench "generate-valid-entry" $ nfIO $ generate (genValid :: Gen Entry)
        , bench "gather" $ nfIO $ gatherEntryWith "ls -lr"
        ]
