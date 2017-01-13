{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Hastory.ChangeDir where

import Introduction

import Hastory.Recent

change :: Int -> IO ()
change ix = do
    recentDirOpts <- getRecentDirOpts
    case recentDirOpts `atMay` ix of
        Nothing -> die "Invalid index choice."
        Just d -> putStrLn d
