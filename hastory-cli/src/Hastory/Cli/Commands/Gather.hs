{-# LANGUAGE FlexibleContexts #-}

module Hastory.Cli.Commands.Gather where

import Control.Monad.Catch
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Control.Monad.Reader
import Data.Text (Text)
import qualified Data.Text.IO as T
import qualified Database.Persist.Sqlite as SQL

import Data.Hastory
import Hastory.Cli.Internal
import Hastory.Cli.OptParse.Types

gather :: (MonadReader Settings m, MonadThrow m, MonadUnliftIO m) => m (Key Entry)
gather = do
  text <- liftIO T.getContents
  gatherFrom text

gatherFrom :: (MonadReader Settings m, MonadThrow m, MonadUnliftIO m) => Text -> m (Key Entry)
gatherFrom text = do
  entry <- liftIO $ gatherEntryWith text
  storeHistory entry

storeHistory :: (MonadReader Settings m, MonadThrow m, MonadUnliftIO m) => Entry -> m (Key Entry)
storeHistory entry = runDb (SQL.insert entry)
