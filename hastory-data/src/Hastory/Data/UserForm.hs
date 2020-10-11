{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Hastory.Data.UserForm where

import Data.Aeson
import Data.Text (Text)
import Data.Validity
import Data.Validity.Text ()
import GHC.Generics

import Hastory.Data.Username

data UserForm =
  UserForm
    { userFormUserName :: Username
    , userFormPassword :: Text
    }
  deriving (Show, Generic)

instance FromJSON UserForm where
  parseJSON = withObject "UserForm" $ \v -> UserForm <$> v .: "userName" <*> v .: "password"

instance ToJSON UserForm where
  toJSON userForm =
    object ["userName" .= userFormUserName userForm, "password" .= userFormPassword userForm]

instance Validity UserForm where
  validate = delve "userFormUserName" . userFormUserName
