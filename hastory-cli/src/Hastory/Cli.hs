{-# LANGUAGE FlexibleContexts #-}

module Hastory.Cli
  ( hastoryCli,
  )
where

import Control.Monad.IO.Unlift (MonadUnliftIO)
import Control.Monad.Reader
import Hastory.Cli.Commands.ChangeDir (change)
import Hastory.Cli.Commands.Gather (gather)
import Hastory.Cli.Commands.GenChangeWrapper (genChangeWrapperScript)
import Hastory.Cli.Commands.GenGatherWrapper (genGatherWrapperScript)
import Hastory.Cli.Commands.ListDir (listRecentDirs)
import Hastory.Cli.Commands.SuggestAlias (suggest)
import Hastory.Cli.Commands.Sync (sync)
import Hastory.Cli.OptParse

hastoryCli :: IO ()
hastoryCli = do
  Instructions d sets <- getInstructions
  runReaderT (dispatch d) sets

dispatch :: (MonadReader Settings m, MonadUnliftIO m) => Dispatch -> m ()
dispatch (DispatchGather _) = void gather
dispatch (DispatchGenGatherWrapperScript _) = liftIO genGatherWrapperScript
dispatch (DispatchChangeDir changeDirSettings) = change changeDirSettings
dispatch (DispatchListRecentDirs lrds) = listRecentDirs lrds
dispatch (DispatchGenChangeWrapperScript _) = liftIO genChangeWrapperScript
dispatch (DispatchSuggestAlias _) = suggest
dispatch (DispatchSync syncSettings) = sync syncSettings
