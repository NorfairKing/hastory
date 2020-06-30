{-# LANGUAGE RecordWildCards #-}

module Data.Hastory.Server.Utils where

import Control.Monad.Error.Class
import Crypto.Hash (Digest, SHA256(..), hashWith)
import qualified Data.ByteString as B
import Data.Hastory.Types
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.Time.Format (defaultTimeLocale, formatTime, iso8601DateFormat)
import Database.Persist
import Path (fromAbsDir)
import Servant.Server

import Data.Hastory.Server.HastoryHandler

withUser :: Username -> (Entity User -> HastoryHandler a) -> HastoryHandler a
withUser username k = do
  mUser <- runDB . getBy $ UniqueUsername username
  case mUser of
    Nothing -> throwError err401
    Just entityUser -> k entityUser

hashEntry :: Entry -> T.Text -> Digest SHA256
hashEntry Entry {..} host = hashWith SHA256 (unifiedData :: B.ByteString)
  where
    unifiedData =
      mconcat
        [ T.encodeUtf8 entryText
        , hashPrepare $ fromAbsDir entryWorkingDir
        , hashPrepare $ formatIso8601 entryDateTime
        , T.encodeUtf8 entryUser
        , T.encodeUtf8 host
        ]
    hashPrepare = T.encodeUtf8 . T.pack
    formatIso8601 = formatTime defaultTimeLocale formatString
    formatString = iso8601DateFormat (Just "%H:%M:%S")

toServerEntry :: SyncRequest -> UserId -> ServerEntry
toServerEntry syncRequest serverEntryUser = ServerEntry {..}
  where
    entry@Entry {..} = syncRequestEntry syncRequest
    serverEntryText = entryText
    serverEntryWorkingDir = entryWorkingDir
    serverEntryDateTime = entryDateTime
    serverEntryHostUser = entryUser
    serverEntryHostName = syncRequestHostName syncRequest
    serverEntryContentHash = hashEntry entry (syncRequestHostName syncRequest)
