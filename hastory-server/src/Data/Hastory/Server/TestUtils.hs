{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Data.Hastory.Server.TestUtils
  ( ServerInfo(..)
  , serverSpec
  , withNewUser
  , withTestServer
  , extractJWTCookie
  , module Network.HTTP.Types
  , module Data.Hastory.Types
  , module SQL
  ) where

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Logger (runNoLoggingT)
import Data.Hastory.Server.HastoryHandler
import Data.Pool (Pool)
import qualified Data.Text as T
import Database.Persist.Sql as SQL
import Database.Persist.Sqlite
  ( SqlBackend
  , fkEnabled
  , mkSqliteConnectionInfo
  , runMigrationSilent
  , runSqlPool
  , withSqlitePoolInfo
  )
import Lens.Micro
import Network.HTTP.Client (defaultManagerSettings, newManager)
import Network.HTTP.Types
import Network.Wai.Handler.Warp (testWithApplication)
import Path
import Path.IO (resolveFile, withSystemTempDir)
import Servant.Auth.Client
import Servant.Auth.Server (defaultCookieSettings, defaultJWTSettings, generateKey)
import Servant.Client
import Test.Hspec
import Test.Hspec.QuickCheck (modifyMaxShrinks, modifyMaxSuccess)

import Data.Hastory.API
import Data.Hastory.Server (Options(..), app)
import Data.Hastory.Types
import Hastory.Server.Data (migrateAll)
import Hastory.Server.Data.PasswordDifficulty

data ServerInfo =
  ServerInfo
    { siClientEnv :: ClientEnv
    , siPool :: Pool SqlBackend
    }

serverSpec :: SpecWith ServerInfo -> Spec
serverSpec = modifyMaxShrinks (const 0) . modifyMaxSuccess (`div` 20) . around withTestServer

withTestServer :: (ServerInfo -> IO a) -> IO a
withTestServer func = do
  manager <- newManager defaultManagerSettings
  withSystemTempDir "hastory-server-test" $ \tmpDir -> do
    dbFile <- resolveFile tmpDir "server.db"
    jwk <- generateKey
    pwDifficulty <- passwordDifficultyOrExit 4
    let jwtSettings = defaultJWTSettings jwk
    runNoLoggingT $
      withSqlitePoolInfo
        (mkSqliteConnectionInfo (T.pack $ fromAbsFile dbFile) & fkEnabled .~ False)
        1 $ \siPool ->
        liftIO $ do
          void $ runSqlPool (runMigrationSilent migrateAll) siPool
          let mkApp = pure $ app opts settings
              opts = Options 10 Nothing
              settings = ServerSettings siPool jwtSettings defaultCookieSettings pwDifficulty
          testWithApplication mkApp $ \p ->
            let siClientEnv = mkClientEnv manager (BaseUrl Http "127.0.0.1" p "")
             in func (ServerInfo {..})

type RegistrationData = (UserId, Token)

withNewUser :: ClientEnv -> UserForm -> (RegistrationData -> Expectation) -> Expectation
withNewUser clientEnv userForm func = do
  Right userId <- runClientM (createUserClient userForm) clientEnv
  Right resp <- runClientM (createSessionClient userForm) clientEnv
  case extractJWTCookie resp of
    Left err -> expectationFailure (show err)
    Right cookie -> func (userId, cookie)
