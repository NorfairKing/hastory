{-# LANGUAGE OverloadedStrings #-}

module Hastory.Cli.Commands.GenGatherWrapperSpec
  ( spec
  ) where

import TestImport

import Hastory.Cli.Commands.GenGatherWrapper
import Hastory.Cli.OptParse.Types ()

spec :: Spec
spec = describe "genScript" $ it "is a fixed value" $ genScript `shouldContain` "FIRST_PROMPT"
