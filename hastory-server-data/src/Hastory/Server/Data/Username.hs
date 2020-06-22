{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Hastory.Server.Data.Username where

import Control.Monad.Fail (MonadFail)
import qualified Control.Monad.Fail as Fail
import GHC.Generics

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
  deriving (Eq, Generic)
  deriving newtype (Show, PersistField, PersistFieldSql)

instance FromJSON Username where
  parseJSON = withText "Username" $ pure . Username

instance ToJSON Username where
  toJSON = toJSON . usernameText

instance Validity Username where
  validate userName = mconcat [check notNull "Username cannot be null", allValidUsernameChar]
    where
      notNull = not . T.null . usernameText $ userName
      allValidUsernameChar =
        decorateList (map UsernameChar $ T.unpack (usernameText userName)) validate

parseUsername :: MonadFail m => Text -> m Username
parseUsername input =
  case parseUsernameWithError input of
    Left err -> Fail.fail err
    Right validatedUsername -> pure validatedUsername

parseUsernameWithError :: Text -> Either String Username
parseUsernameWithError = prettyValidate . Username

validUsernameChar :: Char -> Validation
validUsernameChar c =
  mconcat
    [ declare "The char is not ascii" (isAscii c)
    , declare "The char is not a digit or letter" (isDigit c || isLetter c)
    ]

newtype UsernameChar =
  UsernameChar
    { unUsernameChar :: Char
    }
  deriving (Generic)

instance Validity UsernameChar where
  validate (UsernameChar c) =
    mconcat
      [ check (isAscii c) "The character is not ASCII"
      , check (isLetter c || isDigit c) "The character is not a letter or digit"
      ]
