{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Hastory.ListDir where

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

import Hastory.Internal
import Hastory.Recent
import Hastory.Types

listRecentDirs :: IO ()
listRecentDirs = do
    recentDirOpts <- getRecentDirOpts
    forM_ (zip [0 ..] recentDirOpts) $ \(ix, p) ->
        putStrLn $ unwords [show ix, p]
