module Hastory.Cli.Commands.GenGatherWrapper where

genGatherWrapperScript :: IO ()
genGatherWrapperScript = putStrLn genScript

genScript :: String
genScript =
  unlines
    [ "FIRST_PROMPT=1",
      "function hastory_gather_ {",
      "  AT_PROMPT=1",
      "  if [[ -n \"$FIRST_PROMPT\" ]]; then",
      "    unset FIRST_PROMPT",
      "    return",
      "  fi",
      "  echo $(fc -nl $((HISTCMD - 1))) | hastory gather",
      "}"
    ]
