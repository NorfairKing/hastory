{-# LANGUAGE RecordWildCards #-}

module Data.Hastory.Server.Utils where

import Crypto.Hash (Digest, SHA256(..), hashWith)
import qualified Data.ByteString as B
import Data.Hastory.Types (Entry(..), ServerEntry(..), SyncRequest(..))
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.Time.Format (defaultTimeLocale, formatTime, iso8601DateFormat)
import Path (fromAbsDir)

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

toServerEntry :: SyncRequest -> ServerEntry
toServerEntry syncRequest = ServerEntry {..}
  where
    entry@Entry {..} = syncRequestEntry syncRequest
    serverEntryText = entryText
    serverEntryWorkingDir = entryWorkingDir
    serverEntryDateTime = entryDateTime
    serverEntryUser = entryUser
    serverEntryHostName = syncRequestHostName syncRequest
    serverEntryContentHash = hashEntry entry (syncRequestHostName syncRequest)
