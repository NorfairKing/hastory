{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Hastory.Cli.Internal where

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
import System.Exit

import Hastory.Cli.OptParse.Types
import Hastory.Data.Client.DB

hastoryDir :: MonadReader Settings m => m (Path Abs Dir)
hastoryDir = asks setDataDir

histDir :: MonadReader Settings m => m (Path Abs Dir)
histDir = fmap (</> $(mkRelDir "command-history")) hastoryDir

histDb :: (MonadReader Settings m, MonadUnliftIO m) => m (Path Abs File)
histDb = do
  hd <- histDir
  let filePath = "hastory.db"
  case parseRelFile "hastory.db" of
    Nothing -> liftIO $ die ("Unable to parse relative file path: " <> filePath)
    Just file -> pure $ hd </> file

getLastNDaysOfHistory :: (MonadReader Settings m, MonadUnliftIO m) => Integer -> m [Entry]
getLastNDaysOfHistory n = do
  currentTime <- liftIO getCurrentTime
  let minDateTime = addUTCTime nDaysInPast currentTime
      nDaysInPast = negate $ fromInteger (86400 * n)
  entries <- runDb $ SQL.selectList [EntryDateTime SQL.>=. minDateTime] []
  pure (SQL.entityVal <$> entries)

-- |The 'runDb' function should used, at most, once per command invocation.
-- Please refer hastory <https://github.com/NorfairKing/hastory/issues/35 issue>.
runDb ::
     (MonadReader Settings m, MonadUnliftIO m)
  => ReaderT SqlBackend (NoLoggingT (ResourceT m)) a
  -> m a
runDb dbAction = do
  sets <- ask
  hDb <- liftIO $ runReaderT histDb sets
  ensureDir $ parent hDb
  SQL.runSqlite (T.pack . toFilePath $ hDb) $ do
    SQL.runMigration migrateAll
    dbAction
