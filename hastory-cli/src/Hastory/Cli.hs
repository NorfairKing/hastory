{-# LANGUAGE FlexibleContexts #-}

module Hastory.Cli
  ( hastoryCli
  ) where

import Hastory.Cli.Commands.ChangeDir (change)
import Hastory.Cli.Commands.Gather (gather)
import Hastory.Cli.Commands.GenChangeWrapper (genChangeWrapperScript)
import Hastory.Cli.Commands.GenGatherWrapper (genGatherWrapperScript)
import Hastory.Cli.Commands.ListDir (listRecentDirs)
import Hastory.Cli.Commands.SuggestAlias (suggest)
import Hastory.Cli.OptParse

import Control.Monad.Catch
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Control.Monad.Reader

hastoryCli :: IO ()
hastoryCli = do
  Instructions d sets <- getInstructions
  runReaderT (dispatch d) sets

dispatch :: (MonadReader Settings m, MonadThrow m, MonadUnliftIO m) => Dispatch -> m ()
dispatch DispatchGather = gather
dispatch DispatchGenGatherWrapperScript = liftIO genGatherWrapperScript
dispatch (DispatchChangeDir ix) = change ix
dispatch (DispatchListRecentDirs lrds) = listRecentDirs lrds
dispatch DispatchGenChangeWrapperScript = liftIO genChangeWrapperScript
dispatch DispatchSuggestAlias = suggest
