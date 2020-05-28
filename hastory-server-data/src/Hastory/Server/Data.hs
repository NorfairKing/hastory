{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}

module Hastory.Server.Data where

import Control.Monad.IO.Class (MonadIO)
import Crypto.Hash (Digest, SHA256)
import Data.Aeson (ToJSON (toJSON), object, (.=))
import Data.Password (Password, PasswordHash)
import Data.Password.Bcrypt (Bcrypt, hashPassword)
import Data.Password.Instances ()
import Data.Text (Text)
import Data.Time (UTCTime)
import Database.Persist.TH (mkMigrate, mkPersist, persistLowerCase, share, sqlSettings)
import GHC.Generics (Generic)
import Path (Abs, Dir, Path)

import Data.Hastory.Types.Path ()
import Hastory.Server.Data.Username (Username)
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

User
    name Username sql=citext
    hashedPassword (PasswordHash Bcrypt)
    UniqueUsername name
|]

instance ToJSON User where
  toJSON user = object ["userName" .= userName user]

mkUserPassword :: (MonadIO m) => Password -> m (PasswordHash Bcrypt)
mkUserPassword = hashPassword
