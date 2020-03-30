{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Hastory.Cli.Commands.Gather where

import Control.Monad.Catch
import Control.Monad.Except (runExceptT)
import Control.Monad.Extra (whenJustM)
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Control.Monad.Logger (MonadLogger)
import Control.Monad.Logger.CallStack (logWarn, runNoLoggingT)
import Control.Monad.Reader
import Data.Semigroup ((<>))
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Database.Persist.Sqlite as SQL

import Data.Hastory
import Data.Hastory.API
import Hastory.Cli.Internal
import Hastory.Cli.OptParse.Types

gather :: (MonadReader Settings m, MonadThrow m, MonadUnliftIO m) => m ()
gather = do
  text <- liftIO T.getContents
  gatherFrom text

gatherFrom :: (MonadReader Settings m, MonadThrow m, MonadUnliftIO m) => Text -> m ()
gatherFrom text = do
  entry <- liftIO $ gatherEntryWith text
  storeHistory entry

sendEntryToStorageServer :: (MonadIO m, MonadLogger m) => Entry -> RemoteStorageClientInfo -> m ()
sendEntryToStorageServer entry (RemoteStorageClientInfo url token) =
  runExceptT (mkHastoryClient url token) >>= \case
    Left err -> logWarn $ "Couldn't create the remote storage client: " <> err
    Right client -> do
      resp <- liftIO $ runHastoryClientM client $ \t -> appendCommand t entry
      case resp of
        Left err ->
          logWarn $ "Saving command in remote storage server has failed: " <> T.pack (show err)
        Right _ -> pure ()

storeHistory :: (MonadReader Settings m, MonadThrow m, MonadUnliftIO m) => Entry -> m ()
storeHistory entry = do
  _key <- runDb (SQL.insert entry)
  whenJustM (asks remoteStorageClientInfo) $ runNoLoggingT . sendEntryToStorageServer entry
