{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Data.Hastory.Server.TestUtils
  ( ServerInfo(..)
  , serverSpec
  , withTestServer
  ) where

import Control.Monad.IO.Class
import Control.Monad.Logger (runNoLoggingT)
import qualified Data.Text as T
import Data.Pool (Pool)
import Data.Hastory.Types(migrateAll)
import Control.Monad
import Lens.Micro
import Database.Persist.Sqlite (SqlBackend, runSqlPool, runMigrationSilent, fkEnabled, mkSqliteConnectionInfo, withSqlitePoolInfo)
import Network.HTTP.Client (defaultManagerSettings, newManager)
import Network.Wai.Handler.Warp (testWithApplication)
import Path
import Path.IO (resolveFile, withSystemTempDir)
import Servant.Client (BaseUrl(..), ClientEnv, Scheme(Http), mkClientEnv)
import Test.Hspec
import Test.Hspec.QuickCheck (modifyMaxShrinks, modifyMaxSuccess)

import Data.Hastory.API (Token(..))
import Data.Hastory.Server (Options(..), ServerSettings(..), app, generateToken)

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
    dbFile <- resolveFile tmpDir "server.db"
    runNoLoggingT $
      withSqlitePoolInfo (mkSqliteConnectionInfo (T.pack $ fromAbsFile dbFile) & fkEnabled .~ False ) 1 $ \siPool ->
        liftIO $ do
          void $ runSqlPool (runMigrationSilent migrateAll) siPool
          let mkApp = pure $ app opts settings
              opts = Options 10 Nothing
              settings = ServerSettings siToken siPool
          testWithApplication mkApp $ \p ->
            let siClientEnv = mkClientEnv manager (BaseUrl Http "127.0.0.1" p "")
             in func (ServerInfo {..})
