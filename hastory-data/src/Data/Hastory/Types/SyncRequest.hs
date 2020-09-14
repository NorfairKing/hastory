{-# LANGUAGE DeriveGeneric #-}

module Data.Hastory.Types.SyncRequest where

import Hastory.Cli.Data (Entry(..))

import Data.Aeson
import Data.Text (Text)
import qualified Data.Text as T
import Data.Validity (Validity)
import GHC.Generics (Generic)
import Network.HostName (HostName)

data SyncRequest =
  SyncRequest
    { syncRequestEntries :: [Entry]
    , syncRequestHostName :: Text
    }
  deriving (Show, Eq, Generic)

instance ToJSON SyncRequest

instance FromJSON SyncRequest

instance Validity SyncRequest

toSyncRequest :: [Entry] -> HostName -> SyncRequest
toSyncRequest entries hostName = SyncRequest entries (T.pack hostName)
