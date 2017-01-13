{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Hastory.ChangeDir where

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
import Hastory.OptParse
import Hastory.Recent
import Hastory.Types

change :: Int -> IO ()
change ix = do
    recentDirOpts <- getRecentDirOpts
    case recentDirOpts `atMay` ix of
        Nothing -> die "Invalid index choice."
        Just d -> putStrLn d
