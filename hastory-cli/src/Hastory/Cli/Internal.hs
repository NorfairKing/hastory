{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Hastory.Cli.Internal where

import Data.Hastory
import Hastory.Cli.Data (migrateAll)
import Hastory.Cli.OptParse.Types
import System.Exit

import Control.Monad.Catch
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Control.Monad.Logger (NoLoggingT)
import Control.Monad.Reader
import Control.Monad.Trans.Resource (ResourceT)
import qualified Data.Text as T
import Data.Time
import Database.Persist.Sqlite (SqlBackend)
import qualified Database.Persist.Sqlite as SQL
import Path (Abs, Dir, File, Path, (</>), mkRelDir, parent, parseRelFile, toFilePath)
import Path.IO (ensureDir)

hastoryDir :: MonadReader Settings m => m (Path Abs Dir)
hastoryDir = asks setCacheDir

histDir :: MonadReader Settings m => m (Path Abs Dir)
histDir = fmap (</> $(mkRelDir "command-history")) hastoryDir

histDb :: (MonadThrow m, MonadReader Settings m) => m (Path Abs File)
histDb = do
  hd <- histDir
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

runDb ::
     (MonadThrow m, MonadReader Settings m, MonadUnliftIO m)
  => ReaderT SqlBackend (NoLoggingT (ResourceT m)) a
  -> m a
runDb dbAction = do
  hDb <- histDb
  ensureDir $ parent hDb
  SQL.runSqlite (T.pack . toFilePath $ hDb) $ do
    SQL.runMigration migrateAll
    dbAction

runDb' ::
     (MonadReader Settings m, MonadUnliftIO m)
  => ReaderT SqlBackend (NoLoggingT (ResourceT m)) a
  -> m a
runDb' dbAction = do
  sets <- ask
  case runReaderT histDb sets of
    Nothing -> liftIO $ die "Unable to locate histDb"
    Just hDb -> do
      ensureDir $ parent hDb
      SQL.runSqlite (T.pack . toFilePath $ hDb) $ do
        SQL.runMigration migrateAll
        dbAction
