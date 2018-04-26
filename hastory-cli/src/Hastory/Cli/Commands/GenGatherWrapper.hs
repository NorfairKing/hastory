module Hastory.Cli.Commands.GenGatherWrapper where

import Import

genGatherWrapperScript :: IO ()
genGatherWrapperScript =
    putStrLn $
    unlines
        [ "hastory_gather_ () {"
        , "  echo $(fc -nl $((HISTCMD - 1))) | hastory gather"
        , "}"
        ]
