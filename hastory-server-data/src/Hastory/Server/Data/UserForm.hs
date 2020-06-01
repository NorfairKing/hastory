{-# LANGUAGE OverloadedStrings #-}

module Hastory.Server.Data.UserForm where

import Data.Aeson
import Data.Password
import Data.Password.Instances ()
import qualified Data.Text as T

import Hastory.Server.Data.Username

data UserForm = UserForm { userFormUserName :: Username, userFormPassword :: Password } deriving Show

instance FromJSON UserForm where
  parseJSON = withObject "UserForm" $ \v -> UserForm <$> v .: "userName" <*> v .: "password"

instance ToJSON UserForm where
  toJSON userForm = object ["userName" .= userFormUserName userForm, "password" .= unsafeShowPassword (userFormPassword userForm)]

mkUserForm :: T.Text -> T.Text -> UserForm
mkUserForm userName password = UserForm (mkUsername userName) (mkPassword password)
