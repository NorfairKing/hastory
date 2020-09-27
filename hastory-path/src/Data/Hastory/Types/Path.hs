{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.Hastory.Types.Path
  ( module Data.Hastory.Types.Path
  ) where

import Data.Text (unpack)
import Database.Persist (PersistField(..), PersistValue(..))
import Database.Persist.Sqlite (PersistFieldSql(..), SqlType(..))
import Path as Data.Hastory.Types.Path

instance PersistField (Path Abs Dir) where
  toPersistValue = toPersistValue . toFilePath
  fromPersistValue (PersistText t) =
    case parseAbsDir (unpack t) of
      Left _ -> Left "Unable to marshall Path"
      Right p -> Right p
  fromPersistValue _ = Left "Path must be marshalled from PersistText"

instance PersistFieldSql (Path Abs Dir) where
  sqlType _ = SqlString
