{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Hastory.Server.Data.Username where

import Data.Aeson (FromJSON(parseJSON), ToJSON(toJSON), withText)
import Data.Char
import Data.Text (Text)
import qualified Data.Text as T
import Data.Validity
import Database.Persist.Sql (PersistField, PersistFieldSql)

newtype Username =
  Username
    { usernameText :: Text
    }
  deriving (Eq)
  deriving newtype (Show, PersistField, PersistFieldSql)

instance FromJSON Username where
  parseJSON = withText "Username" $ pure . Username

instance ToJSON Username where
  toJSON = toJSON . usernameText

instance Validity Username where
  validate userName = mconcat [check notNull "Username cannot be null", allAlphaNum]
    where
      notNull = not . T.null . usernameText $ userName
      allAlphaNum =
        decorateList (T.unpack . usernameText $ userName) (declare "is alpha-numeric" . isAlphaNum)
