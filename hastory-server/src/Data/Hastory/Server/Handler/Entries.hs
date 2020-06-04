{-# LANGUAGE RecordWildCards #-}

module Data.Hastory.Server.Handler.Entries where

import Data.Hastory.Server.Handler.Import

createEntryHandler :: ServerSettings -> AuthResult AuthCookie -> SyncRequest -> Handler NoContent
createEntryHandler _ BadPassword _ = unAuthenticated
createEntryHandler _ Indefinite _ = unAuthenticated
createEntryHandler _ NoSuchUser _ = unAuthenticated
createEntryHandler ServerSettings {..} (Authenticated cookie) syncReq = do
  user <- runDB (getBy $ UniqueUsername (unAuthCookie cookie)) _ssDbPool >>= ensureWith err401
  let serverEntry = toServerEntry syncReq (entityKey user)
      upsertIfNotExist = upsertBy uniqueContentHash serverEntry []
      uniqueContentHash = UniqueContentHash (serverEntryContentHash serverEntry)
  _ <- runDB upsertIfNotExist _ssDbPool
  pure NoContent
