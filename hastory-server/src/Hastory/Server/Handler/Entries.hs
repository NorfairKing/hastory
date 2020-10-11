module Hastory.Server.Handler.Entries
  ( createEntryHandler
  ) where

import Hastory.Server.Handler.Import

createEntryHandler :: AuthCookie -> SyncRequest -> HastoryHandler [Entity ServerEntry]
createEntryHandler cookie syncReq =
  withUser (unAuthCookie cookie) $ \user -> do
    _ <- insertNewEntries user syncReq
    fetchEntriesGreaterThan user (syncRequestLogPosition syncReq)

insertNewEntries :: Entity User -> SyncRequest -> HastoryHandler [Entity ServerEntry]
insertNewEntries user syncReq = do
  let serverEntries = toServerEntries syncReq (entityKey user)
  forM serverEntries $ \serverEntry -> runDB $ upsert serverEntry [] --  make no update if record exists

fetchEntriesGreaterThan :: Entity User -> ServerEntryId -> HastoryHandler [Entity ServerEntry]
fetchEntriesGreaterThan user logPosition = runDB query
  where
    query = selectList filters []
    filters = [ServerEntryId >. logPosition, ServerEntryUser ==. entityKey user]
