{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Hastory where

import Introduction

import Hastory.ChangeDir (change)
import Hastory.Gather (gather)
import Hastory.GenChangeWrapper (genChangeWrapperScript)
import Hastory.GenGatherWrapper (genGatherWrapperScript)
import Hastory.ListDir (listRecentDirs)
import Hastory.OptParse

hastory :: IO ()
hastory = do
    (d, Settings) <- getInstructions
    dispatch d

dispatch :: Dispatch -> IO ()
dispatch DispatchGather = gather
dispatch DispatchGenGatherWrapperScript = genGatherWrapperScript
dispatch (DispatchChangeDir ix) = change ix
dispatch DispatchListRecentDirs = listRecentDirs
dispatch DispatchGenChangeWrapperScript = genChangeWrapperScript
