{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Hastory.Server.Data where

import Crypto.Hash (Digest, SHA256)
import Data.Text (Text)
import Data.Time (UTCTime)
import Database.Persist.TH (mkMigrate, mkPersist, persistLowerCase, share, sqlSettings)
import GHC.Generics (Generic)
import Path (Abs, Dir, Path)

import Data.Hastory.Types.Path ()
import Hastory.Server.Digest ()

share
  [mkPersist sqlSettings, mkMigrate "migrateAll"]
  [persistLowerCase|
ServerEntry
    text Text
    workingDir (Path Abs Dir)
    dateTime UTCTime
    user Text
    hostName Text
    contentHash (Digest SHA256)
    UniqueContentHash contentHash
    deriving Show Eq Generic
|]
