{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}

module Hastory.Cli.Commands.ListDir where

import Control.Monad.Catch
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Control.Monad.Reader

import Hastory.Cli.Commands.Recent
import Hastory.Cli.OptParse.Types

listRecentDirs ::
     (MonadReader Settings m, MonadThrow m, MonadUnliftIO m) => ListRecentDirSettings -> m ()
listRecentDirs ListRecentDirSettings {..} = do
  recentDirOpts <- getRecentDirOpts lrdSetBypassCache
  let tups = zip [0 ..] recentDirOpts
      maxlen = maximum $ map (length . show . fst) tups
      formatTup :: Int -> String -> String
      formatTup i s = show i ++ replicate (maxlen - length (show i) + 1) ' ' ++ s
  liftIO $ forM_ tups $ putStrLn . uncurry formatTup
