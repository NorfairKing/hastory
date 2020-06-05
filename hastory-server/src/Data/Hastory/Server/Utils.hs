{-# LANGUAGE RecordWildCards #-}

module Data.Hastory.Server.Utils where

import Control.Monad.Except
import Control.Monad.Reader as X
import Crypto.Hash (Digest, SHA256(..), hashWith)
import qualified Data.ByteString as B
import Data.Hastory.Types
import Data.Pool (Pool)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.Time.Format (defaultTimeLocale, formatTime, iso8601DateFormat)
import Database.Persist.Sql
import Path (fromAbsDir)
import Servant.Auth.Server
import Servant.Server

data ServerSettings =
  ServerSettings
    { _ssDbPool :: Pool SqlBackend
    , _ssJWTSettings :: JWTSettings
    , _ssCookieSettings :: CookieSettings
    }

ensureWith :: MonadError e m => e -> Maybe a -> m a
ensureWith _ (Just a) = pure a
ensureWith e Nothing = throwError e

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

runDB :: ReaderT SqlBackend IO a -> Pool SqlBackend -> Handler a
runDB query pool = liftIO $ runSqlPool query pool

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

unAuthenticated :: Handler a
unAuthenticated = throwError err401
