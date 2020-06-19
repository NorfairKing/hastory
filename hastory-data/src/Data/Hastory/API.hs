{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Data.Hastory.API where

import Control.Monad.Except
import Data.Text (Text)
import Data.Text.Encoding
import Network.HTTP.Client hiding (Proxy)
import Servant
import Servant.Auth.Client
import Servant.Auth.Server hiding (BasicAuth)
import Servant.Client hiding (manager)
import Web.Cookie

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

data HastoryClient =
  HastoryClient
    { hastoryClientEnv :: ClientEnv
    , hastoryClientToken :: Token
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
      userForm = mkUserForm (rawUserName username) password
  res <- liftIO $ runClientM (createSessionClient userForm) clientEnv
  case res of
    Left _ -> pure $ Left UnableToLogin
    Right headers ->
      case extractJWTCookie headers of
        Nothing -> pure $ Left NoJWTTokenFound
        Just token -> pure $ Right (HastoryClient clientEnv token)

-- | Extract token after successful login.
extractJWTCookie :: MonadError () m => Headers AuthCookies NoContent -> m Token
extractJWTCookie headersList =
  case getHeadersHList headersList of
    HCons (Header a) _ -> pure . Token . setCookieValue . parseSetCookie . encodeUtf8 $ a
    _ -> throwError ()

-- | Hastory API client.
--
-- See https://hackage.haskell.org/package/servant-client-0.16.0.1/docs/Servant-Client.html#v:client
createUserClient :: UserForm -> ClientM UserId
createSessionClient :: UserForm -> ClientM (Headers AuthCookies NoContent)
createEntryClient :: Token -> SyncRequest -> ClientM NoContent
(createUserClient :<|> createSessionClient :<|> createEntryClient) = client api

-- | Re-export of runClientM
runHastoryClient :: ClientM a -> ClientEnv -> IO (Either ClientError a)
runHastoryClient = runClientM
