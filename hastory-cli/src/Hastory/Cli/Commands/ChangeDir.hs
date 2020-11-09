{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Hastory.Cli.Commands.ChangeDir where

import Control.Monad.IO.Unlift (MonadUnliftIO)
import Control.Monad.Reader
import Hastory.Cli.Commands.Recent
import Hastory.Cli.OptParse.Types
import Safe (atMay)
import System.Exit (die)

change :: (MonadReader Settings m, MonadUnliftIO m) => ChangeDirSettings -> m ()
change ChangeDirSettings {..} = do
  recentDirOpts <- getRecentDirOpts False
  liftIO $
    case recentDirOpts `atMay` changeDirSetIdx of
      Nothing -> die "Invalid index choice."
      Just d -> putStrLn d
