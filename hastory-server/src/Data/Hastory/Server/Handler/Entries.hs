module Data.Hastory.Server.Handler.Entries where

import Data.Hastory.Server.Handler.Import

createEntryHandler :: AuthCookie -> SyncRequest -> HastoryHandler NoContent
createEntryHandler cookie syncReq =
  withUser (unAuthCookie cookie) $ \user -> do
    let serverEntries = toServerEntries syncReq (entityKey user)
    forM_ serverEntries $ \serverEntry -> do
      let uniqueContentHash = UniqueContentHash (serverEntryContentHash serverEntry)
          upsertIfNotExist = upsertBy uniqueContentHash serverEntry []
      runDB upsertIfNotExist
    pure NoContent

fetchEntryHandler :: AuthCookie -> ServerEntryId -> HastoryHandler [Entity ServerEntry]
fetchEntryHandler cookie position =
  withUser (unAuthCookie cookie) $ \user ->
    runDB $ selectList [ServerEntryId >. position, ServerEntryUser ==. entityKey user] []
