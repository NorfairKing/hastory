{-# LANGUAGE OverloadedStrings #-}

module Hastory.Cli.Commands.GenGatherWrapperSpec
  ( spec,
  )
where

import Hastory.Cli.Commands.GenGatherWrapper
import Hastory.Cli.OptParse.Types ()
import TestImport

spec :: Spec
spec = describe "genScript" $ it "is a fixed value" $ genScript `shouldContain` "FIRST_PROMPT"
