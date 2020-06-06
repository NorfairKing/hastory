{-# LANGUAGE RecordWildCards #-}

module Data.Hastory.Server.Handler.Entries where

import Data.Hastory.Server.Handler.Import

createEntryHandler :: ServerSettings -> AuthCookie -> SyncRequest -> Handler NoContent
createEntryHandler ServerSettings {..} cookie syncReq = do
  user <- runDB (getBy $ UniqueUsername (unAuthCookie cookie)) _ssDbPool >>= ensureWith err401
  let serverEntry = toServerEntry syncReq (entityKey user)
      upsertIfNotExist = upsertBy uniqueContentHash serverEntry []
      uniqueContentHash = UniqueContentHash (serverEntryContentHash serverEntry)
  _ <- runDB upsertIfNotExist _ssDbPool
  pure NoContent
