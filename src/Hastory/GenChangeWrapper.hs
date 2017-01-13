module Hastory.GenChangeWrapper where

import Introduction

genChangeWrapperScript :: IO ()
genChangeWrapperScript =
    putStrLn $
    unlines
        [ "hastory_change_directory_ () {"
        , "  local args=\"$@\""
        , "  if [[ \"$args\" == \"\" ]]"
        , "  then"
        , "    hastory list-recent-directories"
        , "  else"
        , "    local dir=$(hastory change-directory \"$args\")"
        , "    cd dir"
        , "  fi"
        , "}"
        ]
