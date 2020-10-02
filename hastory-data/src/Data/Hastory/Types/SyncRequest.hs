{-# LANGUAGE DeriveGeneric #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleInstances #-}

module Data.Hastory.Types.SyncRequest where

import Hastory.Cli.Data (Entry(..))
import Hastory.Server.Data (ServerEntryId)

import Data.Aeson
import Data.Text (Text)
import qualified Data.Text as T
import Data.Validity
import GHC.Generics (Generic)
import Network.HostName (HostName)

data SyncRequest =
  SyncRequest
    { syncRequestEntries :: [Entry]
    , syncRequestHostName :: Text
    , syncRequestLogPosition :: ServerEntryId
    }
  deriving (Show, Eq, Generic)

instance ToJSON SyncRequest

instance FromJSON SyncRequest

instance Validity ServerEntryId where
  validate = trivialValidation

instance Validity SyncRequest where
  validate SyncRequest {..} =
    mconcat
      [ delve "entries" syncRequestEntries
      , check (T.length syncRequestHostName > 0) "hostname is at least one char"
      , delve "log position" syncRequestLogPosition
      ]

toSyncRequest :: [Entry] -> HostName -> ServerEntryId -> SyncRequest
toSyncRequest entries hostName = SyncRequest entries (T.pack hostName)
