{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Hastory.API where

import Control.Monad.Except
import Data.Text (Text)
import Data.Text.Encoding
import Database.Persist
import Hastory.Data
import Hastory.Data.Server.DB (ServerEntry, UserId)
import Network.HTTP.Client hiding (Proxy)
import Servant
import Servant.Auth.Client
import Servant.Auth.Server hiding (BasicAuth)
import Servant.Client hiding (manager)
import Web.Cookie

type RequiredQueryParam = QueryParam' '[Required, Strict]

type AuthCookies = '[Header "Set-Cookie" Text]

type EntriesPost =
  "entries" :> ReqBody '[JSON] SyncRequest :> PostCreated '[JSON] [Entity ServerEntry]

type Protected = Auth '[JWT] AuthCookie

type UsersAPI = "users" :> ReqBody '[JSON] UserForm :> PostCreated '[JSON] UserId

type Sessions =
  "sessions" :> ReqBody '[JSON] UserForm :> Verb 'POST 204 '[JSON] (Headers AuthCookies NoContent)

-- | Main Hastory API specification.
type HastoryAPI = UsersAPI :<|> Sessions :<|> (Protected :> EntriesPost)

-- | Proxy for Hastory API.
api :: Proxy HastoryAPI
api = Proxy

data HastoryClient
  = HastoryClient
      { hastoryClientEnv :: ClientEnv,
        hastoryClientToken :: Token
      }

-- | An ADT that encodes possible failures for mkHastoryClient
data ClientEnvFailure
  = UnableToLogin
  | NoJWTTokenFound
  deriving (Show)

-- | Creates a hastory client type.
--
-- This type is needed for two reasons. First, because creating and destroying
-- HTTP managers are expensive. Secondly, the user should only have to log in
-- successfully once.
--
-- Once a user gets a HastoryClient, it's being used throughout the entire life of the user.
mkHastoryClient ::
  MonadIO m => BaseUrl -> Username -> Text -> m (Either ClientEnvFailure HastoryClient)
mkHastoryClient url username password = do
  manager <- liftIO $ newManager defaultManagerSettings
  let clientEnv = mkClientEnv manager url
      userForm = UserForm username password
  res <- liftIO $ runClientM (createSessionClient userForm) clientEnv
  case res of
    Left _ -> pure $ Left UnableToLogin
    Right headers ->
      case extractJWTCookie headers of
        Left err -> pure $ Left err
        Right token -> pure $ Right (HastoryClient clientEnv token)

mkUnauthenticatedClient :: MonadIO m => BaseUrl -> m ClientEnv
mkUnauthenticatedClient url = do
  manager <- liftIO $ newManager defaultManagerSettings
  pure $ mkClientEnv manager url

-- | Extract token after successful login.
extractJWTCookie :: Headers AuthCookies NoContent -> Either ClientEnvFailure Token
extractJWTCookie headersList =
  case getHeadersHList headersList of
    HCons (Header a) _ -> pure . Token . setCookieValue . parseSetCookie . encodeUtf8 $ a
    _ -> Left NoJWTTokenFound

-- | Hastory API client.
--
-- See https://hackage.haskell.org/package/servant-client-0.16.0.1/docs/Servant-Client.html#v:client
createUserClient :: UserForm -> ClientM UserId

createSessionClient :: UserForm -> ClientM (Headers AuthCookies NoContent)

createEntryClient :: Token -> SyncRequest -> ClientM [Entity ServerEntry]
(createUserClient :<|> createSessionClient :<|> createEntryClient) = client api

-- | Re-export of runClientM
runHastoryClient :: ClientM a -> ClientEnv -> IO (Either ClientError a)
runHastoryClient = runClientM
