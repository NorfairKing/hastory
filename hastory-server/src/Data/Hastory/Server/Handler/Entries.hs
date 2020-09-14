module Data.Hastory.Server.Handler.Entries where

import Data.Hastory.Server.Handler.Import

createEntryHandler :: AuthCookie -> SyncRequest -> HastoryHandler NoContent
createEntryHandler cookie syncReq =
  withUser (unAuthCookie cookie) $ \user -> do
    let serverEntries = toServerEntries syncReq (entityKey user)
    forM_ serverEntries $ \serverEntry -> do
      let upsertIfNotExist = upsertBy uniqueContentHash serverEntry []
          uniqueContentHash = UniqueContentHash (serverEntryContentHash serverEntry)
      runDB upsertIfNotExist
    pure NoContent
