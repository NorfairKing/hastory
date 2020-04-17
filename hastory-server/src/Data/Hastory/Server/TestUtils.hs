{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Data.Hastory.Server.TestUtils
  ( ServerInfo(..)
  , serverSpec
  , withTestServer
  ) where

import Control.Monad.Logger (runNoLoggingT)
import Data.Pool (Pool)
import Database.Persist.Sqlite (SqlBackend)
import Network.HTTP.Client (defaultManagerSettings, newManager)
import Network.Wai.Handler.Warp (testWithApplication)
import Path.IO (resolveFile, withSystemTempDir)
import Servant.Client (BaseUrl(..), ClientEnv, Scheme(Http), mkClientEnv)
import Test.Hspec
import Test.Hspec.QuickCheck (modifyMaxShrinks, modifyMaxSuccess)

import Data.Hastory.API (Token(..))
import Data.Hastory.Server (Options(..), ServerSettings(..), app, generateToken, prepareDb)

data ServerInfo =
  ServerInfo
    { siClientEnv :: ClientEnv
    , siToken :: Token
    , siPool :: Pool SqlBackend
    }

serverSpec :: SpecWith ServerInfo -> Spec
serverSpec = modifyMaxShrinks (const 0) . modifyMaxSuccess (`div` 20) . around withTestServer

withTestServer :: (ServerInfo -> IO a) -> IO a
withTestServer func = do
  manager <- newManager defaultManagerSettings
  siToken <- generateToken
  withSystemTempDir "hastory-server-test" $ \tmpDir -> do
    filePath <- resolveFile tmpDir "server.db"
    siPool <- runNoLoggingT $ prepareDb filePath
    let mkApp = pure $ app opts settings
        opts = Options 10 Nothing
        settings = ServerSettings siToken siPool
    testWithApplication mkApp $ \p ->
      let siClientEnv = mkClientEnv manager (BaseUrl Http "127.0.0.1" p "")
       in func (ServerInfo {..})
