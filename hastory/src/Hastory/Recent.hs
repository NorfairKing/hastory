{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
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
import Hastory.OptParse.Types
import Hastory.Types

getRecentDirOpts ::
       (MonadIO m, MonadThrow m, MonadReader Settings m) => Bool -> m [FilePath]
getRecentDirOpts bypassCache =
    if bypassCache
        then recompute
        else do
            cacheFile <- recentDirsCacheFile
            mcontents <-
                liftIO $ forgivingAbsence $ LB.readFile $ toFilePath cacheFile
            case mcontents of
                Nothing -> recompute
                Just contents ->
                    case JSON.eitherDecode contents of
                        Left _ -> recompute -- If the file is corrupt, just don't care.
                        Right RecentDirOptsCache {..} -> do
                            now <- liftIO Time.getZonedTime
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
        unless bypassCache $ cacheRecentDirOpts recentDirs
        pure recentDirs

cacheInvalidationDuration :: NominalDiffTime
cacheInvalidationDuration = 10 -- seconds

computeRecentDirOpts ::
       (MonadIO m, MonadThrow m, MonadReader Settings m) => m [FilePath]
computeRecentDirOpts = do
    rawEnts <- getLastNDaysOfHistory 7
    home <- liftIO getHomeDir
    let entries = filter ((/= home) . entryWorkingDir) rawEnts
    now <- liftIO Time.getZonedTime
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
    doCountsWith conv func = foldl go HM.empty
      where
        go hm k = HM.alter a (conv k) hm
          where
            a Nothing = Just 0
            a (Just d) = Just $ d + func k

cacheRecentDirOpts :: (MonadIO m, MonadReader Settings m) => [FilePath] -> m ()
cacheRecentDirOpts fs = do
    now <- liftIO Time.getZonedTime
    let cache = RecentDirOptsCache {cacheTimestamp = now, cacheRecentDirs = fs}
    cacheFile <- recentDirsCacheFile
    liftIO $ LB.writeFile (toFilePath cacheFile) $ JSON.encodePretty cache

recentDirsCacheFile :: MonadReader Settings m => m (Path Abs File)
recentDirsCacheFile =
    fmap (</> $(mkRelFile "recent-dirs-cache.json")) hastoryDir

data RecentDirOptsCache = RecentDirOptsCache
    { cacheTimestamp :: ZonedTime
    , cacheRecentDirs :: [FilePath]
    } deriving (Show, Generic)

instance ToJSON RecentDirOptsCache

instance FromJSON RecentDirOptsCache
