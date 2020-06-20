module Data.Hastory.Server.HastoryHandler
  ( module Data.Hastory.Server.HastoryHandler
  , module Control.Monad.Reader
  ) where

import Control.Monad.Reader
import Data.Pool
import Database.Persist.Sql
import Servant
import Servant.Auth.Server

type HastoryHandler a = ReaderT ServerSettings Handler a

data ServerSettings =
  ServerSettings
    { serverSetPool :: Pool SqlBackend
    , serverSetJWTSettings :: JWTSettings
    , serverSetCookieSettings :: CookieSettings
    }

type Query a = ReaderT SqlBackend IO a

runDB :: Query a -> HastoryHandler a
runDB query = do
  pool <- asks serverSetPool
  liftIO $ runSqlPool query pool

unAuthenticated :: HastoryHandler a
unAuthenticated = throwError err401
