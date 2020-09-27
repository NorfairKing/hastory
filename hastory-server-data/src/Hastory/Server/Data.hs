{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Hastory.Server.Data where

-- import           Path                         (Abs, Dir, Path)
import Data.Text (Text)
import Data.Time (UTCTime)
import Database.Persist.TH (mkMigrate, mkPersist, persistLowerCase, share, sqlSettings)
import GHC.Generics (Generic)

import Data.Hastory.Types.Digest
import Data.Hastory.Types.Path
import Hastory.Server.Data.Password (Bcrypt, PasswordHash)
import Hastory.Server.Data.Username (Username)

share
  [mkPersist sqlSettings, mkMigrate "migrateAll"]
  [persistLowerCase|

User
    name Username
    hashedPassword (PasswordHash Bcrypt)
    UniqueUsername name

ServerEntry json
    user UserId
    text Text
    workingDir (Path Abs Dir)
    dateTime UTCTime
    hostUser Text
    hostName Text
    contentHash (Digest SHA256)
    UniqueContentHash contentHash
    deriving Show Eq Generic

|]
