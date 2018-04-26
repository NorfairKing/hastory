{-# LANGUAGE FlexibleContexts #-}

module Hastory.Cli
    ( hastoryCli
    ) where

import Import

import Hastory.Cli.Commands.ChangeDir (change)
import Hastory.Cli.Commands.Gather (gather)
import Hastory.Cli.Commands.GenChangeWrapper (genChangeWrapperScript)
import Hastory.Cli.Commands.GenGatherWrapper (genGatherWrapperScript)
import Hastory.Cli.Commands.ListDir (listRecentDirs)
import Hastory.Cli.Commands.SuggestAlias (suggest)
import Hastory.Cli.OptParse

hastoryCli :: IO ()
hastoryCli = do
    Instructions d sets <- getInstructions
    runReaderT (dispatch d) sets

dispatch ::
       (MonadIO m, MonadThrow m, MonadReader Settings m) => Dispatch -> m ()
dispatch DispatchGather = gather
dispatch DispatchGenGatherWrapperScript = liftIO genGatherWrapperScript
dispatch (DispatchChangeDir ix) = change ix
dispatch (DispatchListRecentDirs lrds) = listRecentDirs lrds
dispatch DispatchGenChangeWrapperScript = liftIO genChangeWrapperScript
dispatch DispatchSuggestAlias = suggest
