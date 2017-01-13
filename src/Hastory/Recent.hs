{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Hastory.Recent where

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
import Hastory.Types

docounts
    :: (Eq a, Eq b, Hashable a, Hashable b)
    => (a -> b) -> [a] -> HashMap b Double
docounts f = doCountsWith f (const 1)

doCountsWith
    :: (Eq a, Eq b, Hashable a, Hashable b)
    => (a -> b) -> (a -> Double) -> [a] -> HashMap b Double
doCountsWith conv func es = foldl go HM.empty es
  where
    go hm k = HM.alter a (conv k) hm
      where
        a Nothing = Just 0
        a (Just d) = Just $ d + func k

getRecentDirOpts :: IO [FilePath]
getRecentDirOpts = do
    rawEnts <- getHistory
    home <- getHomeDir
    let entries = filter ((/= home) . entryWorkingDir) rawEnts
    now <- Time.getZonedTime
    let dateFunc entry = 1 / d
          where
            d =
                realToFrac $
                Time.diffUTCTime
                    (Time.zonedTimeToUTC now)
                    (Time.zonedTimeToUTC $ entryDateTime entry)
    let counts = doCountsWith (toFilePath . entryWorkingDir) dateFunc entries
    let tups = reverse $ sortOn snd $ HM.toList counts
    pure $ take 10 $ map fst tups
