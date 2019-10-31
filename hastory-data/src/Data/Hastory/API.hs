{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeOperators       #-}

module Data.Hastory.API where

import Control.Exception.Lifted (catch)
import Control.Monad.Catch (MonadThrow)
import Control.Monad.Except (MonadError, throwError)
import Control.Monad.IO.Class
import Control.Monad.Trans.Control (MonadBaseControl)
import Data.Semigroup ((<>))
import Data.String (IsString, fromString)
import qualified Data.Text as T
import GHC.TypeLits (symbolVal)
import Network.HTTP.Client (defaultManagerSettings, newManager)
import Prelude
import Servant
import Servant.Client (BaseUrl, ClientEnv (ClientEnv), ClientM, client,
                       mkClientEnv, runClientM)
import Servant.Client.Core (InvalidBaseUrlException)
import Servant.Client.Core.Reexport (ServantError, parseBaseUrl)

import Data.Hastory.Types (EntryWithKey)

-- * Hastory API

-- | Header key to use while authorizing users via tokens.
--
-- Every hastory client are expected to provide a header like the following:
--
-- X-Token: abcd1234abcd1234abcd
type TokenHeaderKey = "X-Token"

-- | Helper for getting the token header in the runtime.
tokenHeaderKey :: IsString s => s
tokenHeaderKey = fromString $ symbolVal (Proxy @TokenHeaderKey)

-- | Token for authenticating Hastory Server users.
newtype Token = Token T.Text

instance FromHttpApiData Token where
  parseHeader = fmap Token . parseHeader
  parseUrlPiece = fmap Token . parseUrlPiece

instance ToHttpApiData Token where
  toUrlPiece (Token token) = toUrlPiece token

-- | Main Hastory API specification.
type HastoryAPI = "commands" :> "append" :> Header TokenHeaderKey Token :> ReqBody '[JSON] EntryWithKey :> Post '[JSON] ()

api :: Proxy HastoryAPI
api = Proxy

data HastoryClient = HastoryClient ClientEnv Token

instance Show HastoryClient where
  show (HastoryClient (ClientEnv _ baseUrl _) _) = "HastoryClient with baseUrl " <> show baseUrl

-- | By default parseBaseUrl throws exceptions and we don't like exceptions here.
-- Therefore we convert it to an action in IO and MonadError T.Text context.
parseBaseUrlSafe :: (MonadThrow m, MonadBaseControl IO m, MonadError T.Text m) => T.Text -> m BaseUrl
parseBaseUrlSafe url =
  parseBaseUrl (T.unpack url)
    `catch` (\(_ :: InvalidBaseUrlException) -> throwError "Base url couldn't be parsed")

-- * Hastory Client

-- | Creates a hastory client type.
--
-- This type is needed because creating & destroying HTTP managers are expensive.
-- Once a user gets a HastoryClient, it's being used throughout the entire life of the user.
mkHastoryClient :: (MonadError T.Text m, MonadThrow m, MonadBaseControl IO m, MonadIO m) => T.Text -> Token -> m HastoryClient
mkHastoryClient baseUrl token = do
  parsedBaseUrl <- parseBaseUrlSafe baseUrl
  manager <- liftIO $ newManager defaultManagerSettings
  let clientEnv = mkClientEnv manager parsedBaseUrl
  pure $ HastoryClient clientEnv token

-- | Hastory API client.
--
-- Adding more methods in the future:
-- `appendCommand :<|> method2 :<|> method3 = client api`
--
-- See https://hackage.haskell.org/package/servant-client-0.16.0.1/docs/Servant-Client.html#v:client
appendCommand :: Maybe Token -> EntryWithKey -> ClientM ()
appendCommand = client api


-- | Run a hastory API method that requires passing a token by using
-- the existing token in HastoryClient. Since we create HastoryClient once, we save
-- the token so that users won't have to deal with tokens afterwards.
runHastoryClientM :: HastoryClient -> (Token -> ClientM a) -> IO (Either ServantError a)
runHastoryClientM (HastoryClient clientEnv token) action =
  runClientM (action token) clientEnv
