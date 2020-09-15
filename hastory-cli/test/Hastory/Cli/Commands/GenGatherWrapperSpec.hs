{-# LANGUAGE OverloadedStrings #-}

module Hastory.Cli.Commands.GenGatherWrapperSpec
  ( spec
  ) where

import TestImport

-- import           Data.Hastory
import Hastory.Cli.Commands.GenGatherWrapper
import Hastory.Cli.OptParse.Types ()

-- import           Servant.Client.Core
spec :: Spec
spec = describe "genScript" $ it "is a fixed value" $ genScript `shouldContain` "FIRST_PROMPT"
