{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Hastory.Server.Data.UserForm where

import Data.Aeson
import qualified Data.Text as T
import Data.Validity
import Data.Validity.Text ()
import GHC.Generics

import Hastory.Server.Data.Username

data UserForm =
  UserForm
    { userFormUserName :: Username
    , userFormPassword :: T.Text
    }
  deriving (Show, Generic)

instance FromJSON UserForm where
  parseJSON = withObject "UserForm" $ \v -> UserForm <$> v .: "userName" <*> v .: "password"

instance ToJSON UserForm where
  toJSON userForm =
    object ["userName" .= userFormUserName userForm, "password" .= userFormPassword userForm]

instance Validity UserForm

mkUserForm :: T.Text -> T.Text -> UserForm
mkUserForm userName = UserForm (mkUsername userName)
