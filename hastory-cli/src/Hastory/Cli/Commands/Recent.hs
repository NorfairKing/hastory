{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}

module Hastory.Cli.Commands.Recent
    ( getRecentDirOpts
    ) where

import Import

import Data.Hastory
import Hastory.Cli.Internal
import Hastory.Cli.OptParse.Types
import Hastory.Cli.Utils (doCountsWith)

import Data.Aeson as JSON
import Data.Aeson.Encode.Pretty as JSON
import Data.Time.Clock (NominalDiffTime)
import Data.Time.LocalTime (ZonedTime)

import qualified Data.ByteString.Lazy as LB
import qualified Data.HashMap.Lazy as HM
import qualified Data.Time.Clock as Time
import qualified Data.Time.LocalTime as Time

getRecentDirOpts ::
       (MonadReader Settings m, MonadThrow m, MonadUnliftIO m) => Bool -> m [FilePath]
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
       (MonadReader Settings m, MonadThrow m, MonadUnliftIO m) => m [FilePath]
computeRecentDirOpts = do
    rawEnts <- getLastNDaysOfHistory 7
    home <- liftIO getHomeDir
    let entries = filter ((/= home) . entryWorkingDir) rawEnts
    now <- liftIO Time.getCurrentTime
    let dateFunc entry = 1 / d
          where
            d =
                realToFrac $
                Time.diffUTCTime now (entryDateTime entry)
    let counts = doCountsWith (toFilePath . entryWorkingDir) dateFunc entries
    let tups = reverse $ sortOn snd $ HM.toList counts
    pure $ take 10 $ map fst tups

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
