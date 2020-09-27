{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.Hastory.Types.Digest
  ( module Data.Hastory.Types.Digest
  ) where

import Crypto.Hash as Data.Hastory.Types.Digest
import Data.Aeson
import Data.ByteArray (convert)
import qualified Data.ByteArray as BA
import qualified Data.ByteString as B
import qualified Data.ByteString.Base64 as Base64
import Data.Proxy (Proxy(Proxy))
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Database.Persist.Class (PersistField(fromPersistValue, toPersistValue))
import Database.Persist.Sql (PersistFieldSql(sqlType))
import Database.Persist.Types (PersistValue(PersistByteString))

instance HashAlgorithm a => PersistField (Digest a) where
  toPersistValue = toPersistValue . B.pack . BA.unpack
  fromPersistValue (PersistByteString rawDigest) =
    case digestFromByteString rawDigest of
      Nothing -> Left "Unable to reify Digest from ByteString"
      Just reifiedDigest -> Right reifiedDigest
  fromPersistValue _ = Left "Digest values must be convered from PersistByteString"

instance HashAlgorithm a => PersistFieldSql (Digest a) where
  sqlType _ = sqlType (Proxy :: Proxy B.ByteString)

instance FromJSON (Digest SHA256) where
  parseJSON =
    withText "Digest SHA256" $ \text -> do
      let failureMsg err = fail ("Failed to parse (Digest SHA256): " ++ T.unpack err)
      bytestring <- either failureMsg pure (Base64.decodeBase64 $ T.encodeUtf8 text)
      maybe (failureMsg text) pure (digestFromByteString bytestring)

instance ToJSON (Digest SHA256) where
  toJSON = toJSON . T.decodeUtf8 . Base64.encodeBase64' . convert
