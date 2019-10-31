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
type HastoryAPI = "commands" :> "append" :> Header TokenHeaderKey Token :> ReqBody '[JSON] EntryWithKey :> Post '[JSON] ()

api :: Proxy HastoryAPI
api = Proxy

data HastoryClient = HastoryClient ClientEnv Token

instance Show HastoryClient where
  show (HastoryClient (ClientEnv _ baseUrl _) _) = "HastoryClient with baseUrl " <> show baseUrl

parseBaseUrlSafe :: (MonadThrow m, MonadBaseControl IO m, MonadError T.Text m) => T.Text -> m BaseUrl
parseBaseUrlSafe url =
  parseBaseUrl (T.unpack url)
    `catch` (\(_ :: InvalidBaseUrlException) -> throwError "Base url couldn't be parsed")

-- * Hastory Client
mkHastoryClient :: (MonadError T.Text m, MonadThrow m, MonadBaseControl IO m, MonadIO m) => T.Text -> Token -> m HastoryClient
mkHastoryClient baseUrl token = do
  parsedBaseUrl <- parseBaseUrlSafe baseUrl
  manager <- liftIO $ newManager defaultManagerSettings
  let clientEnv = mkClientEnv manager parsedBaseUrl
  pure $ HastoryClient clientEnv token

appendCommand :: Maybe Token -> EntryWithKey -> ClientM ()
appendCommand = client api

runHastoryClientM :: HastoryClient -> (Token -> ClientM a) -> IO (Either ServantError a)
runHastoryClientM (HastoryClient clientEnv token) action =
  runClientM (action token) clientEnv
