{-# LANGUAGE RecordWildCards #-}

module Data.Hastory.Server.Utils where

import Crypto.Hash (Digest, SHA256(..), hashWith)
import qualified Data.ByteString as B
import Data.Hastory.Types (Entry(..), ServerEntry(..), SyncRequest(..))
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

hashEntry :: Entry -> T.Text -> Digest SHA256
hashEntry Entry {..} host = hashWith SHA256 (unifiedData :: B.ByteString)
  where
    unifiedData =
      mconcat
        [ T.encodeUtf8 entryText
        , (hashPrepare . show) entryWorkingDir
        , (hashPrepare . show) entryDateTime
        , T.encodeUtf8 entryUser
        , T.encodeUtf8 host
        ]
    hashPrepare = T.encodeUtf8 . T.pack

toServerEntry :: SyncRequest -> ServerEntry
toServerEntry syncRequest = ServerEntry cmd workingDir dateTime user hostName contentHash
  where
    cmd = entryText entry
    workingDir = entryWorkingDir entry
    dateTime = entryDateTime entry
    user = entryUser entry
    hostName = syncRequestHostName syncRequest
    contentHash = hashEntry entry hostName
    entry = syncRequestEntry syncRequest
