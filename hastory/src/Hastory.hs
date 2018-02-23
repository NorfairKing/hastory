{-# LANGUAGE FlexibleContexts #-}

module Hastory where

import Import

import Hastory.ChangeDir (change)
import Hastory.Gather (gather)
import Hastory.GenChangeWrapper (genChangeWrapperScript)
import Hastory.GenGatherWrapper (genGatherWrapperScript)
import Hastory.ListDir (listRecentDirs)
import Hastory.OptParse
import Hastory.SuggestAlias (suggest)

hastory :: IO ()
hastory = do
    (d, sets) <- getInstructions
    runReaderT (dispatch d) sets

dispatch ::
       (MonadIO m, MonadThrow m, MonadReader Settings m) => Dispatch -> m ()
dispatch DispatchGather = gather
dispatch DispatchGenGatherWrapperScript = liftIO genGatherWrapperScript
dispatch (DispatchChangeDir ix) = change ix
dispatch (DispatchListRecentDirs lrds) = listRecentDirs lrds
dispatch DispatchGenChangeWrapperScript = liftIO genChangeWrapperScript
dispatch DispatchSuggestAlias = suggest
