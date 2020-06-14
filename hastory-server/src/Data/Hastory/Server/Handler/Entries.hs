module Data.Hastory.Server.Handler.Entries where

import Data.Hastory.Server.Handler.Import

createEntryHandler :: AuthCookie -> SyncRequest -> HastoryHandler NoContent
createEntryHandler cookie syncReq = do
  user <- (runDB . getBy $ UniqueUsername (unAuthCookie cookie)) >>= ensureWith err401
  let serverEntry = toServerEntry syncReq (entityKey user)
      upsertIfNotExist = upsertBy uniqueContentHash serverEntry []
      uniqueContentHash = UniqueContentHash (serverEntryContentHash serverEntry)
  _ <- runDB upsertIfNotExist
  pure NoContent
