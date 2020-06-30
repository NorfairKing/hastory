module Data.Hastory.Server.Handler.Entries where

import Data.Hastory.Server.Handler.Import

createEntryHandler :: AuthCookie -> SyncRequest -> HastoryHandler NoContent
createEntryHandler cookie syncReq =
  withUser (unAuthCookie cookie) $ \user -> do
    let upsertIfNotExist = upsertBy uniqueContentHash serverEntry []
        uniqueContentHash = UniqueContentHash (serverEntryContentHash serverEntry)
        serverEntry = toServerEntry syncReq (entityKey user)
    _ <- runDB upsertIfNotExist
    pure NoContent
