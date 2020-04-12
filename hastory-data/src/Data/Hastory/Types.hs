{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}

module Data.Hastory.Types where

import Data.Hastory.Types.Path ()

import Control.DeepSeq
import Data.Aeson
import Data.GenValidity
import Data.GenValidity.Path ()
import Data.GenValidity.Text ()
import Data.GenValidity.Time ()
import Data.Hashable
import Data.Hashable.Time ()
import Data.Text (Text)
import Data.Time
import Data.Validity ()
import Data.Validity.Path ()
import Data.Validity.Text ()
import Data.Validity.Time.Clock ()
import Database.Persist.TH (mkMigrate, mkPersist, persistLowerCase, share, sqlSettings)
import GHC.Generics (Generic)
import Path

share
  [mkPersist sqlSettings, mkMigrate "migrateAll"]
  [persistLowerCase|
Entry
    text Text
    workingDir (Path Abs Dir)
    dateTime UTCTime
    hostName Text
    user Text
    deriving Show Eq Generic
|]

instance NFData Entry

instance Hashable Entry

instance Validity Entry

instance GenValid Entry where
  genValid = genValidStructurally

  shrinkValid = shrinkValidStructurally

instance ToJSON Entry

instance FromJSON Entry
