{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}

module Hastory.ListDir where

import Import

import Hastory.OptParse.Types
import Hastory.Recent

listRecentDirs :: (MonadIO m, MonadReader Settings m) => m ()
listRecentDirs = do
    recentDirOpts <- getRecentDirOpts
    let tups = zip [0 ..] recentDirOpts
        maxlen = maximum $ map (length . show . fst) tups
        formatTup :: Int -> String -> String
        formatTup i s =
            show i ++ replicate (maxlen - length (show i) + 1) ' ' ++ s
    liftIO $ forM_ tups $ putStrLn . uncurry formatTup
