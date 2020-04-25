{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE OverloadedStrings #-}

module Hastory.Server.Digest where

import Crypto.Hash (Digest, HashAlgorithm, digestFromByteString)
import qualified Data.ByteArray as BA
import qualified Data.ByteString as B
import Data.Proxy (Proxy(Proxy))
import Database.Persist.Class (PersistField(fromPersistValue, toPersistValue))
import Database.Persist.Sql (PersistFieldSql(sqlType))
import Database.Persist.Types (PersistValue(PersistByteString))

instance HashAlgorithm a => PersistField (Digest a) where
  toPersistValue = toPersistValue . B.pack . BA.unpack
  fromPersistValue (PersistByteString rawDigest) =
    maybe (Left "Unable to reify Digest from ByteString") Right (digestFromByteString rawDigest)
  fromPersistValue _ = Left "Digest values must be convered from PersistByteString"

instance HashAlgorithm a => PersistFieldSql (Digest a) where
  sqlType _ = sqlType (Proxy :: Proxy B.ByteString)
