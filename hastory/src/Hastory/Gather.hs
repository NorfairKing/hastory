{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module Hastory.Gather where

import           Import

import qualified Data.Aeson             as JSON
import qualified Data.ByteString.Lazy   as LB
import           Data.Text              (Text)
import qualified Data.Text.IO           as T
import qualified Data.Time.LocalTime    as Time
import           Network.HostName       (getHostName)
import           System.Posix.User      (getEffectiveUserName)

import           Hastory.Internal
import           Hastory.OptParse.Types
import           Hastory.Types

gather :: (MonadIO m, MonadThrow m, MonadReader Settings m) => m ()
gather = do
    liftIO $ putStrLn "gathering!"
    text <- liftIO T.getContents
    gatherFrom text

gatherFrom :: (MonadIO m, MonadThrow m, MonadReader Settings m) => Text -> m ()
gatherFrom text = do
    entry <- liftIO $ getEntryWith text
    storeHistory entry

getEntryWith :: Text -> IO Entry
getEntryWith text = do
    curtime <- Time.getZonedTime
    curdir <- getCurrentDir
    hostname <- getHostName
    user <- getEffectiveUserName
    pure
        Entry
        { entryText = text
        , entryDateTime = curtime
        , entryWorkingDir = curdir
        , entryHostName = hostname
        , entryUser = user
        }

sendEntryToStorageServer :: (MonadIO m, MonadThrow m, MonadReader Settings m) => Entry -> m ()
sendEntryToStorageServer entry =
  asks storageServer >>= \case
    Nothing -> pure ()
    Just url ->
      liftIO $ print $ "Storing in server: " <> url

storeHistory ::
       (MonadIO m, MonadThrow m, MonadReader Settings m) => Entry -> m ()
storeHistory entry = do
    hFile <- histFileFor $ entryDateTime entry
    sendEntryToStorageServer entry
    liftIO $ do
        ensureDir $ parent hFile
        LB.appendFile (toFilePath hFile) $ JSON.encode entry <> "\n"
