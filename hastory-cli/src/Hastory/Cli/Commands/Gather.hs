{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Hastory.Cli.Commands.Gather where

import Import

import qualified Data.Aeson as JSON
import qualified Data.ByteString.Lazy as LB
import Data.Text (Text)
import qualified Data.Text.IO as T

import Data.Hastory

import Hastory.Cli.Internal
import Hastory.Cli.OptParse.Types

gather :: (MonadIO m, MonadThrow m, MonadReader Settings m) => m ()
gather = do
    text <- liftIO T.getContents
    gatherFrom text

gatherFrom :: (MonadIO m, MonadThrow m, MonadReader Settings m) => Text -> m ()
gatherFrom text = do
    entry <- liftIO $ gatherEntryWith text
    storeHistory entry

storeHistory ::
       (MonadIO m,  MonadReader Settings m) => Entry -> m ()
storeHistory entry = do
    hFile <- histFileFor $ entryDateTime entry
    liftIO $ do
        ensureDir $ parent hFile
        LB.appendFile (toFilePath hFile) $ JSON.encode entry <> "\n"
