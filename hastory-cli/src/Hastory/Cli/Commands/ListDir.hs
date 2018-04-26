{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}

module Hastory.Cli.Commands.ListDir where

import Import

import Hastory.Cli.OptParse.Types
import Hastory.Cli.Commands.Recent

listRecentDirs ::
       (MonadIO m,  MonadReader Settings m)
    => ListRecentDirSets
    -> m ()
listRecentDirs ListRecentDirSets {..} = do
    recentDirOpts <- getRecentDirOpts lrdSetBypassCache
    let tups = zip [0 ..] recentDirOpts
        maxlen = maximum $ map (length . show . fst) tups
        formatTup :: Int -> String -> String
        formatTup i s =
            show i ++ replicate (maxlen - length (show i) + 1) ' ' ++ s
    liftIO $ forM_ tups $ putStrLn . uncurry formatTup
