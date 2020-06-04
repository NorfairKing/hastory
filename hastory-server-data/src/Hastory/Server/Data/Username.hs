{-# LANGUAGE OverloadedStrings #-}

module Hastory.Server.Data.Username where

import Data.Aeson (FromJSON(parseJSON), ToJSON(toJSON), withText)
import qualified Data.CaseInsensitive as CI
import Data.Proxy (Proxy(Proxy))
import qualified Data.Text as T
import Data.Validity
import Database.Persist (PersistField(fromPersistValue, toPersistValue), PersistValue(PersistText))
import Database.Persist.Sql (PersistFieldSql(sqlType))

newtype Username =
  Username
    { unUsername :: CI.CI T.Text
    }
  deriving (Eq)

instance PersistField Username where
  toPersistValue = toPersistValue . rawUserName
  fromPersistValue (PersistText userName) = Right $ mkUsername userName
  fromPersistValue _ = Left "Username value must be converted from PersistText"

instance PersistFieldSql Username where
  sqlType _ = sqlType (Proxy :: Proxy T.Text)

instance Show Username where
  show = show . rawUserName

instance FromJSON Username where
  parseJSON = withText "Username" $ pure . mkUsername

instance ToJSON Username where
  toJSON = toJSON . rawUserName

instance Validity Username where
  validate userName = check notNull "Username cannot be null"
    where
      notNull = not . T.null . rawUserName $ userName

mkUsername :: T.Text -> Username
mkUsername = Username . CI.mk

rawUserName :: Username -> T.Text
rawUserName = CI.original . unUsername
