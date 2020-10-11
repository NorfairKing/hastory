{-# LANGUAGE RecordWildCards #-}

module Hastory.Server.Utils where

import Control.Monad.Error.Class
import Crypto.Hash (Digest, SHA256(..), hashWith)
import Data.ByteString (ByteString)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.Time.Format (defaultTimeLocale, formatTime, iso8601DateFormat)
import Database.Persist
import Path (fromAbsDir)
import Servant.Auth.Server
import Servant.Server

import Hastory.Data
import Hastory.Data.Client.DB
import Hastory.Data.Server.DB
import Hastory.Server.HastoryHandler

withUser :: Username -> (Entity User -> HastoryHandler a) -> HastoryHandler a
withUser username k = do
  mUser <- runDB . getBy $ UniqueUsername username
  case mUser of
    Nothing -> throwError err401
    Just entityUser -> k entityUser

withSetCookie :: Username -> (ByteString -> HastoryHandler a) -> HastoryHandler a
withSetCookie userName func = do
  let cookie = AuthCookie userName
  cookieSettings <- asks serverSetCookieSettings
  jwtSettings <- asks serverSetJWTSettings
  mSetCookie <- liftIO (makeSessionCookieBS cookieSettings jwtSettings cookie)
  case mSetCookie of
    Nothing -> throwError err401
    Just bs -> func bs

hashEntry :: Entry -> T.Text -> Digest SHA256
hashEntry Entry {..} host = hashWith SHA256 (unifiedData :: ByteString)
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

toServerEntries :: SyncRequest -> UserId -> [ServerEntry]
toServerEntries syncRequest serverEntryUser =
  let syncEntries = syncRequestEntries syncRequest
      hostName = syncRequestHostName syncRequest
   in map (toServerEntry serverEntryUser hostName) syncEntries

toServerEntry :: UserId -> Text -> Entry -> ServerEntry
toServerEntry serverEntryUser serverEntryHostName entry@Entry {..} = ServerEntry {..}
  where
    serverEntryText = entryText
    serverEntryWorkingDir = entryWorkingDir
    serverEntryDateTime = entryDateTime
    serverEntryHostUser = entryUser
    serverEntryContentHash = hashEntry entry serverEntryHostName
