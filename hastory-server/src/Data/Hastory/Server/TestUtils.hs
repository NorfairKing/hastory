{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Data.Hastory.Server.TestUtils
  ( ServerInfo(..)
  , serverSpec
  , withTestServer
  ) where

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Logger (runNoLoggingT)
import Data.Hastory.Server (Options (..), ServerSettings (..), app)
import Data.Pool (Pool)
import qualified Data.Text as T
import Database.Persist.Sqlite (SqlBackend, fkEnabled, mkSqliteConnectionInfo, runMigrationSilent,
                                runSqlPool, withSqlitePoolInfo)
import Hastory.Server.Data (migrateAll)
import Lens.Micro
import Network.HTTP.Client (defaultManagerSettings, newManager)
import Network.Wai.Handler.Warp (testWithApplication)
import Path
import Path.IO (resolveFile, withSystemTempDir)
import Servant.Client (BaseUrl (..), ClientEnv, Scheme (Http), mkClientEnv)
import Test.Hspec
import Test.Hspec.QuickCheck (modifyMaxShrinks, modifyMaxSuccess)

data ServerInfo =
  ServerInfo
    { siClientEnv :: ClientEnv
    , siPool      :: Pool SqlBackend
    }

serverSpec :: SpecWith ServerInfo -> Spec
serverSpec = modifyMaxShrinks (const 0) . modifyMaxSuccess (`div` 20) . around withTestServer

withTestServer :: (ServerInfo -> IO a) -> IO a
withTestServer func = do
  manager <- newManager defaultManagerSettings
  withSystemTempDir "hastory-server-test" $ \tmpDir -> do
    dbFile <- resolveFile tmpDir "server.db"
    runNoLoggingT $
      withSqlitePoolInfo
        (mkSqliteConnectionInfo (T.pack $ fromAbsFile dbFile) & fkEnabled .~ False)
        1 $ \siPool ->
        liftIO $ do
          void $ runSqlPool (runMigrationSilent migrateAll) siPool
          let mkApp = pure $ app opts settings
              opts = Options 10 Nothing
              settings = ServerSettings siPool
          testWithApplication mkApp $ \p ->
            let siClientEnv = mkClientEnv manager (BaseUrl Http "127.0.0.1" p "")
             in func (ServerInfo {..})
