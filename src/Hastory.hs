{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Hastory where

import Introduction

import qualified Data.Aeson as JSON
import qualified Data.ByteString as SB
import qualified Data.ByteString.Lazy as LB
import qualified Data.ByteString.Lazy.Char8 as LB8
import Data.HashMap.Lazy (HashMap)
import qualified Data.HashMap.Lazy as HM
import Data.Hashable (Hashable)
import Data.List (unwords)
import qualified Data.Text.IO as T
import qualified Data.Time.Clock as Time
import qualified Data.Time.LocalTime as Time

import Hastory.ChangeDir
import Hastory.Recent
import Hastory.Gather
import Hastory.Internal
import Hastory.OptParse
import Hastory.Types

hastory :: IO ()
hastory = do
    (d, Settings) <- getInstructions
    dispatch d

dispatch :: Dispatch -> IO ()
dispatch DispatchGather = gather
dispatch (DispatchChangeDir ix) = change ix
dispatch DispatchListRecentDirs = listRecentDirs
dispatch DispatchGenChangeWrapperScript = genChangeWrapperScript

listRecentDirs :: IO ()
listRecentDirs = do
    recentDirOpts <- getRecentDirOpts
    forM_ (zip [0 ..] recentDirOpts) $ \(ix, p) ->
        putStrLn $ unwords [show ix, p]

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
