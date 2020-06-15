{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Data.Hastory.API where

import Data.Text (Text)

import Servant
import Servant.Auth.Client
import Servant.Auth.Server hiding (BasicAuth)
import Servant.Client

import Data.Hastory.Types

type AuthCookies = '[ Header "Set-Cookie" Text]

type EntriesAPI = "entries" :> ReqBody '[ JSON] SyncRequest :> PostCreated '[ JSON] NoContent

type ProtectedAPI = Auth '[ JWT] AuthCookie

type UsersAPI = "users" :> ReqBody '[ JSON] UserForm :> PostCreated '[ JSON] UserId

type SessionsAPI
   = "sessions" :> ReqBody '[ JSON] UserForm :> Verb 'POST 204 '[ JSON] (Headers AuthCookies NoContent)

-- | Main Hastory API specification.
type HastoryAPI = UsersAPI :<|> SessionsAPI :<|> (ProtectedAPI :> EntriesAPI)

-- | Proxy for Hastory API.
api :: Proxy HastoryAPI
api = Proxy

-- | Hastory API client.
--
-- See https://hackage.haskell.org/package/servant-client-0.16.0.1/docs/Servant-Client.html#v:client
createUserClient :: UserForm -> ClientM UserId
createSessionClient :: UserForm -> ClientM (Headers AuthCookies NoContent)
createEntryClient :: Token -> SyncRequest -> ClientM NoContent
(createUserClient :<|> createSessionClient :<|> createEntryClient) = client api
