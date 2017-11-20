{-# LANGUAGE FlexibleContexts #-}

module Hastory.SuggestAlias where

import Import

import Hastory.OptParse.Types

suggest :: (MonadIO m, MonadThrow m, MonadReader Settings m) => m ()
suggest = liftIO $ die "Suggest Alias here"
