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

import Crypto.Hash (Digest, SHA256)
import Data.Aeson
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (UTCTime)
import Database.Persist.TH (mkMigrate, mkPersist, persistLowerCase, share, sqlSettings)
import GHC.Generics (Generic)
import Path (Abs, Dir, Path)
import Text.Read

import Data.Hastory.Types.Path ()
import Hastory.Server.Data.Password (Bcrypt, PasswordHash)
import Hastory.Server.Data.Username (Username)
import Hastory.Server.Digest ()

instance FromJSON (Digest SHA256) where
  parseJSON =
    withText "Digest SHA256" $ \text ->
      case readMaybe (T.unpack text) of
        Nothing -> fail ("Failed to parse (Digest SHA256): " ++ T.unpack text)
        Just digest -> pure digest

instance ToJSON (Digest SHA256) where
  toJSON :: Digest SHA256 -> Value
  toJSON = toJSON . show

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
