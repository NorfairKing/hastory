{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Data.Hastory.API where

import Control.Monad.Except (MonadError, liftEither)
import Control.Monad.IO.Class
import Data.Bifunctor (first)
import Data.String (IsString, fromString)
import qualified Data.Text as T
import GHC.TypeLits (symbolVal)
import Network.HTTP.Client (defaultManagerSettings, newManager)
import Prelude
import Servant
import Servant.Client (ClientEnv, ClientM, client, mkClientEnv, runClientM)
import Servant.Client.Core.Reexport (ServantError, parseBaseUrl)

import Data.Hastory.Types (Entry)

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
newtype Token =
  Token T.Text
  deriving (Show, Eq)

instance FromHttpApiData Token where
  parseHeader = fmap Token . parseHeader
  parseUrlPiece = fmap Token . parseUrlPiece

instance ToHttpApiData Token where
  toUrlPiece (Token token) = toUrlPiece token

-- | Main Hastory API specification.
type HastoryAPI
   = "commands" :> "append" :> Header TokenHeaderKey Token :> ReqBody '[ JSON] Entry :> Post '[ JSON] ()

api :: Proxy HastoryAPI
api = Proxy

data HastoryClient =
  HastoryClient ClientEnv Token

-- * Hastory Client
-- | Creates a hastory client type.
--
-- This type is needed because creating & destroying HTTP managers are expensive.
-- Once a user gets a HastoryClient, it's being used throughout the entire life of the user.
mkHastoryClient :: (MonadError T.Text m, MonadIO m) => T.Text -> Token -> m HastoryClient
mkHastoryClient baseUrl token = do
  parsedBaseUrl <- liftEither $ first (T.pack . show) $ parseBaseUrl (T.unpack baseUrl)
  manager <- liftIO $ newManager defaultManagerSettings
  let clientEnv = mkClientEnv manager parsedBaseUrl
  pure $ HastoryClient clientEnv token

-- | Hastory API client.
--
-- Adding more methods in the future:
-- `appendCommand :<|> method2 :<|> method3 = client api`
--
-- See https://hackage.haskell.org/package/servant-client-0.16.0.1/docs/Servant-Client.html#v:client
appendCommand :: Maybe Token -> Entry -> ClientM ()
appendCommand = client api

-- | Run a hastory API method that requires passing a token by using
-- the existing token in HastoryClient. Since we create HastoryClient once, we save
-- the token so that users won't have to deal with tokens afterwards.
runHastoryClientM :: HastoryClient -> (Token -> ClientM a) -> IO (Either ServantError a)
runHastoryClientM (HastoryClient clientEnv token) action = runClientM (action token) clientEnv
