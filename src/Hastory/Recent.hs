{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}

module Hastory.Recent
    ( getRecentDirOpts
    ) where

import Import

import Data.Aeson as JSON
import Data.Aeson.Encode.Pretty as JSON
import qualified Data.ByteString.Lazy as LB
import Data.HashMap.Lazy (HashMap)
import qualified Data.HashMap.Lazy as HM
import Data.Hashable (Hashable)
import qualified Data.Time.Clock as Time
import Data.Time.Clock (NominalDiffTime)
import qualified Data.Time.LocalTime as Time
import Data.Time.LocalTime (ZonedTime)

import Hastory.Internal
import Hastory.Types

getRecentDirOpts :: IO [FilePath]
getRecentDirOpts = do
    cacheFile <- recentDirsCacheFile
    mcontents <- forgivingAbsence $ LB.readFile $ toFilePath cacheFile
    case mcontents of
        Nothing -> recompute
        Just contents ->
            case JSON.eitherDecode contents of
                Left _ -> recompute -- If the file is corrupt, just don't care.
                Right RecentDirOptsCache {..} -> do
                    now <- Time.getZonedTime
                    if Time.diffUTCTime
                           (Time.zonedTimeToUTC now)
                           (Time.zonedTimeToUTC cacheTimestamp) >
                       cacheInvalidationDuration
                        then recompute
                        else do
                            cacheRecentDirOpts cacheRecentDirs
                            pure cacheRecentDirs
  where
    recompute = do
        recentDirs <- computeRecentDirOpts
        cacheRecentDirOpts recentDirs
        pure recentDirs

cacheInvalidationDuration :: NominalDiffTime
cacheInvalidationDuration = 10 -- seconds

computeRecentDirOpts :: IO [FilePath]
computeRecentDirOpts = do
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
  where
    doCountsWith ::
           (Eq b, Hashable b)
        => (a -> b)
        -> (a -> Double)
        -> [a]
        -> HashMap b Double
    doCountsWith conv func es = foldl go HM.empty es
      where
        go hm k = HM.alter a (conv k) hm
          where
            a Nothing = Just 0
            a (Just d) = Just $ d + func k

cacheRecentDirOpts :: [FilePath] -> IO ()
cacheRecentDirOpts fs = do
    now <- Time.getZonedTime
    let cache = RecentDirOptsCache {cacheTimestamp = now, cacheRecentDirs = fs}
    cacheFile <- recentDirsCacheFile
    LB.writeFile (toFilePath cacheFile) $ JSON.encodePretty cache

recentDirsCacheFile :: IO (Path Abs File)
recentDirsCacheFile =
    fmap (</> $(mkRelFile "recent-dirs-cache.json")) hastoryDir

data RecentDirOptsCache = RecentDirOptsCache
    { cacheTimestamp :: ZonedTime
    , cacheRecentDirs :: [FilePath]
    } deriving (Show, Generic)

instance ToJSON RecentDirOptsCache

instance FromJSON RecentDirOptsCache
