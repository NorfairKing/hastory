{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Hastory.Data.Client.DB where

import Control.DeepSeq
import Data.Aeson
import Data.GenValidity.Path ()
import Data.GenValidity.Text ()
import Data.GenValidity.Time ()
import Data.Int
import Data.Text (Text)
import Data.Time (UTCTime)
import Data.Validity (Validity)
import Database.Persist.TH (mkMigrate, mkPersist, persistLowerCase, share, sqlSettings)
import GHC.Generics (Generic)

import Hastory.Data.Path

share
  [mkPersist sqlSettings, mkMigrate "migrateAll"]
  [persistLowerCase|
Entry
    text Text
    workingDir (Path Abs Dir)
    dateTime UTCTime
    user Text
    syncWitness Int64 Maybe
    EntryData text workingDir dateTime user
    hostName Text Maybe
    deriving Show Eq Generic
|]

instance Validity Entry

instance NFData Entry

instance ToJSON Entry

instance FromJSON Entry
