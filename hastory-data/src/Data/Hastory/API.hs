{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}

module Data.Hastory.API where

import Servant
import Servant.Client (ClientM, client)

import Data.Hastory.Types (UserForm)

type UserAPI = "users" :> ReqBody '[JSON] UserForm :> PostCreated '[JSON] ()

-- | Main Hastory API specification.
type HastoryAPI
   = UserAPI


api :: Proxy HastoryAPI
api = Proxy

-- | Hastory API client.
--
-- See https://hackage.haskell.org/package/servant-client-0.16.0.1/docs/Servant-Client.html#v:client
createUserClient :: UserForm -> ClientM ()
createUserClient = client api
