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

import Crypto.Hash (Digest, SHA256, digestFromByteString)
import Data.Aeson
import Data.ByteArray (convert)
import qualified Data.ByteString.Base64 as Base64
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.Time (UTCTime)
import Database.Persist.TH (mkMigrate, mkPersist, persistLowerCase, share, sqlSettings)
import GHC.Generics (Generic)
import Path (Abs, Dir, Path)

import Data.Hastory.Types.Path ()
import Hastory.Server.Data.Password (Bcrypt, PasswordHash)
import Hastory.Server.Data.Username (Username)
import Hastory.Server.Digest ()

instance FromJSON (Digest SHA256) where
  parseJSON =
    withText "Digest SHA256" $ \text -> do
      let failureMsg err = fail ("Failed to parse (Digest SHA256): " ++ T.unpack err)
      bytestring <- either failureMsg pure (Base64.decodeBase64 $ T.encodeUtf8 text)
      maybe (failureMsg text) pure (digestFromByteString bytestring)

instance ToJSON (Digest SHA256) where
  toJSON :: Digest SHA256 -> Value
  toJSON = toJSON . T.decodeUtf8 . Base64.encodeBase64' . convert

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
