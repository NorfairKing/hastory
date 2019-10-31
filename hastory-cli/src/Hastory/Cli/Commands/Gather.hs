{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

module Hastory.Cli.Commands.Gather where

import Control.Monad.Catch
import Control.Monad.Extra (whenJustM)
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Control.Monad.Logger (MonadLogger)
import Control.Monad.Logger.CallStack (logWarn, runStdoutLoggingT)
import Control.Monad.Reader
import Data.Int (Int64)
import Data.Semigroup ((<>))
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Database.Persist.Sqlite as SQL

import Data.Hastory
import Data.Hastory.API
import Data.Hastory.Types (EntryWithKey (..))
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

sendEntryToStorageServer :: (MonadIO m, MonadLogger m) => (SQL.Key Entry, Entry) -> HastoryClient -> m ()
sendEntryToStorageServer (key, entry) client =
  case findEntryId of
    Nothing ->
      logWarn "Entry is expected to have a single Int64 value as it's primary key."
    Just entryId -> do
      resp <- liftIO $ runHastoryClientM client $ \t -> appendCommand (Just t) (EntryWithKey entryId entry)
      case resp of
        Left err -> logWarn $ "Saving command in remote storage server has failed: " <> T.pack (show err)
        Right _ -> pure ()
  where
    findEntryId :: Maybe Int64
    findEntryId =
      case SQL.keyToValues key of
        [SQL.PersistInt64 rowId] -> Just rowId
        _                        -> Nothing

storeHistory ::
       (MonadReader Settings m, MonadThrow m, MonadUnliftIO m) => Entry -> m ()
storeHistory entry = do
  key <- runDb (SQL.insert entry)
  whenJustM (asks remoteStorageClient) $
    runStdoutLoggingT . sendEntryToStorageServer (key, entry)
