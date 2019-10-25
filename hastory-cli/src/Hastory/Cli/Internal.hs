{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}

module Hastory.Cli.Internal where

import Import

import Data.Hastory
import Data.Time
import Hastory.Cli.OptParse.Types

import Control.Monad.IO.Unlift (MonadUnliftIO)
import Control.Monad.Logger (NoLoggingT)
import Control.Monad.Trans.Resource (ResourceT)
import Database.Persist.Sqlite (SqlBackend)

import qualified Data.Text as T
import qualified Database.Persist.Sqlite as SQL

hastoryDir :: MonadReader Settings m => m (Path Abs Dir)
hastoryDir = asks setCacheDir

histDir :: MonadReader Settings m => m (Path Abs Dir)
histDir = fmap (</> $(mkRelDir "command-history")) hastoryDir

histDb :: (MonadThrow m, MonadReader Settings m) => m (Path Abs File)
histDb = do
  hd   <- histDir
  file <- parseRelFile "hastory.db"
  pure $ hd </> file

getLastNDaysOfHistory ::
       (MonadReader Settings m, MonadThrow m, MonadUnliftIO m) => Integer -> m [Entry]
getLastNDaysOfHistory n = do
    currentTime <- liftIO getCurrentTime
    let minDateTime = addUTCTime nDaysInPast currentTime
        nDaysInPast = negate $ fromInteger (86400 * n)
    entries <- runDb $ SQL.selectList [EntryDateTime SQL.>=. minDateTime] []
    pure (SQL.entityVal <$> entries)

runDb :: (MonadThrow m, MonadReader Settings m, MonadUnliftIO m) => ReaderT SqlBackend (NoLoggingT (ResourceT m)) a -> m a
runDb dbAction = do
  hDb <- histDb
  ensureDir $ parent hDb
  SQL.runSqlite (T.pack . toFilePath $ hDb) $ do
    SQL.runMigration migrateAll
    dbAction
