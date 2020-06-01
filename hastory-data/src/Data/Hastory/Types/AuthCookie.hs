{-# LANGUAGE DeriveGeneric #-}

module Data.Hastory.Types.AuthCookie where

import Data.Aeson
import GHC.Generics (Generic)
import Servant.Auth.Server

import Hastory.Server.Data.Username

newtype AuthCookie = AuthCookie { unAuthCookie :: Username } deriving Generic

instance ToJSON AuthCookie
instance FromJSON AuthCookie

instance ToJWT AuthCookie
instance FromJWT AuthCookie
