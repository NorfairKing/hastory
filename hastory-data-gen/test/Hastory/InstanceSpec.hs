{-# LANGUAGE TypeApplications #-}

module Hastory.InstanceSpec
  ( spec,
  )
where

import Hastory.Data.Client.DB
import Hastory.Gen ()
import TestImport

spec :: Spec
spec =
  describe "Entry" $ do
    eqSpecOnValid @Entry
    arbitrarySpec @Entry
    genValidSpec @Entry
