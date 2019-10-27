{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeOperators       #-}

module Data.Hastory.API where

import Data.Semigroup ((<>))
import Data.String (IsString, fromString)
import qualified Data.Text as T
import GHC.TypeLits (symbolVal)
import Network.HTTP.Client (defaultManagerSettings, newManager)
import Prelude
import Servant
import Servant.Client (ClientEnv (ClientEnv), ClientM, client, mkClientEnv,
                       runClientM)
import Servant.Client.Core.Reexport

import Data.Hastory.Types (Entry)

-- * Hastory API

-- | TODO: Document
type TokenHeaderKey = "X-Token"

-- | TODO: Document
tokenHeaderKey :: IsString s => s
tokenHeaderKey = fromString $ symbolVal (Proxy @TokenHeaderKey)

-- | TODO: Document
newtype Token = Token T.Text

-- | TODO: Document
instance FromHttpApiData Token where
  parseHeader = fmap Token . parseHeader
  parseUrlPiece = fmap Token . parseUrlPiece

instance ToHttpApiData Token where
  toUrlPiece (Token token) = toUrlPiece token

-- | TODO: Document
type HastoryAPI = "commands" :> "append" :> Header TokenHeaderKey Token :> ReqBody '[JSON] Entry :> Post '[JSON] ()

api :: Proxy HastoryAPI
api = Proxy

data HastoryClient = HastoryClient ClientEnv Token

instance Show HastoryClient where
  show (HastoryClient (ClientEnv _ baseUrl _) _) = "HastoryClient with baseUrl " <> show baseUrl

-- * Hastory Client
-- TODO: parseBaseUrl throws exceptions. Handle it.
mkHastoryClient :: T.Text -> Token -> IO HastoryClient
mkHastoryClient baseUrl token = do
  parsedBaseUrl <- parseBaseUrl (T.unpack baseUrl)
  manager <- newManager defaultManagerSettings
  let clientEnv = mkClientEnv manager parsedBaseUrl
  pure $ HastoryClient clientEnv token

appendCommand :: Maybe Token -> Entry -> ClientM ()
appendCommand = client api

runHastoryClientM :: HastoryClient -> (Token -> ClientM a) -> IO (Either ServantError a)
runHastoryClientM (HastoryClient clientEnv token) action =
  runClientM (action token) clientEnv
