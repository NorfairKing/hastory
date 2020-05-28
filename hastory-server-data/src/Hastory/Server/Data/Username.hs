{-# LANGUAGE OverloadedStrings #-}

module Hastory.Server.Data.Username where

import Data.Aeson (FromJSON (parseJSON), ToJSON (toJSON), withText)
import qualified Data.CaseInsensitive as CI
import Data.Proxy (Proxy (Proxy))
import qualified Data.Text as T
import Database.Persist (PersistField (fromPersistValue, toPersistValue),
                         PersistValue (PersistText))
import Database.Persist.Sql (PersistFieldSql (sqlType))

newtype Username = Username { unUsername :: CI.CI T.Text } deriving Eq

instance PersistField Username where
  toPersistValue = toPersistValue . CI.original . unUsername

  fromPersistValue (PersistText userName) = Right $ Username (CI.mk userName)
  fromPersistValue _ = Left "Username value must be converted from PersistText"

instance PersistFieldSql Username where
  sqlType _ = sqlType (Proxy :: Proxy T.Text)

instance Show Username where
  show = show . unUsername

instance FromJSON Username where
  parseJSON = withText "Username" $ pure . Username . CI.mk

instance ToJSON Username where
  toJSON = toJSON . CI.original . unUsername

mkUsername :: T.Text -> Username
mkUsername = Username . CI.mk
