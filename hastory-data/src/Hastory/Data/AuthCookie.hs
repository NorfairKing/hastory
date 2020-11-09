{-# LANGUAGE DeriveGeneric #-}

module Hastory.Data.AuthCookie where

import Data.Aeson
import GHC.Generics (Generic)
import Hastory.Data.Username
import Servant.Auth.Server

newtype AuthCookie
  = AuthCookie
      { unAuthCookie :: Username
      }
  deriving (Generic)

instance ToJSON AuthCookie

instance FromJSON AuthCookie

instance ToJWT AuthCookie

instance FromJWT AuthCookie
