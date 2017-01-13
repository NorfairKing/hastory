module Hastory.GenGatherWrapper where

import Introduction

genGatherWrapperScript :: IO ()
genGatherWrapperScript =
    putStrLn $
    unlines
        [ "hastory_gather_ () {"
        , "  echo $(fc -nl $((HISTCMD - 1))) | hastory gather"
        , "}"
        ]
