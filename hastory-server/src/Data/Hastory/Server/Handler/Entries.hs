module Data.Hastory.Server.Handler.Entries
  ( createEntryHandler
  ) where

import Data.Hastory.Server.Handler.Import

createEntryHandler ::
     AuthCookie -> SyncRequest -> ServerEntryId -> HastoryHandler [Entity ServerEntry]
createEntryHandler cookie syncReq logPosition =
  withUser (unAuthCookie cookie) $ \user ->
    insertNewEntries user syncReq >> fetchEntriesGreaterThan user logPosition

insertNewEntries :: Entity User -> SyncRequest -> HastoryHandler [Entity ServerEntry]
insertNewEntries user syncReq = do
  let serverEntries = toServerEntries syncReq (entityKey user)
  forM serverEntries $ \serverEntry -> do
    let uniqueContentHash = UniqueContentHash (serverEntryContentHash serverEntry)
        upsertIfNotExist = upsertBy uniqueContentHash serverEntry []
    runDB upsertIfNotExist

fetchEntriesGreaterThan :: Entity User -> ServerEntryId -> HastoryHandler [Entity ServerEntry]
fetchEntriesGreaterThan user logPosition = runDB query
  where
    query = selectList filters []
    filters = [ServerEntryId >. logPosition, ServerEntryUser ==. entityKey user]
