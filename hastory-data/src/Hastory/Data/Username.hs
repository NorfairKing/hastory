{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Hastory.Data.Username where

import qualified Control.Monad.Fail as Fail
import Data.Aeson (FromJSON (parseJSON), ToJSON (toJSON), withText)
import Data.Char
import Data.Text (Text)
import qualified Data.Text as T
import Data.Validity
import Database.Persist.Sql (PersistField, PersistFieldSql)
import GHC.Generics

newtype Username
  = Username
      { usernameText :: Text
      }
  deriving (Eq, Generic)
  deriving newtype (Show, PersistField, PersistFieldSql)

instance FromJSON Username where
  parseJSON = withText "Username" $ pure . Username

instance ToJSON Username where
  toJSON = toJSON . usernameText

instance Validity Username where
  validate userName =
    mconcat
      [check (userNameLength > 2) "Username length is greater than two", allAsciiLetterOrDigit]
    where
      userNameLength = T.length . usernameText $ userName
      allAsciiLetterOrDigit = decorateList (T.unpack . usernameText $ userName) asciiLetterOrDigit
      asciiLetterOrDigit c =
        mconcat
          [ check (isAscii c) "Char is ASCII",
            check (isDigit c || isLetter c) "Char is letter or digit"
          ]

parseUsername :: MonadFail m => Text -> m Username
parseUsername input =
  case parseUsernameWithError input of
    Left err -> Fail.fail err
    Right validatedUsername -> pure validatedUsername

parseUsernameWithError :: Text -> Either String Username
parseUsernameWithError = prettyValidate . Username
