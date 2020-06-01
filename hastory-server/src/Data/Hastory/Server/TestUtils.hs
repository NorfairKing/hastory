{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Data.Hastory.Server.TestUtils
  ( ServerInfo(..)
  , serverSpec
  , withNewUser
  , withTestServer
  ) where

import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Logger     (runNoLoggingT)
import qualified Data.ByteString.Char8    as B
import           Data.Hastory.Server      (Options (..), ServerSettings (..),
                                           app)
import           Data.List
import           Data.Pool                (Pool)
import qualified Data.Text                as T
import           Database.Persist.Sqlite  (SqlBackend, fkEnabled,
                                           mkSqliteConnectionInfo,
                                           runMigrationSilent, runSqlPool,
                                           withSqlitePoolInfo)
import           Hastory.Server.Data      (migrateAll)
import           Lens.Micro
import           Network.HTTP.Client      (defaultManagerSettings, newManager)
import           Network.Wai.Handler.Warp (testWithApplication)
import           Path
import           Path.IO                  (resolveFile, withSystemTempDir)
import           Servant.API
import           Servant.Auth.Client      (Token (Token))
import           Servant.Auth.Server      (defaultCookieSettings,
                                           defaultJWTSettings, generateKey)
import           Servant.Client           (BaseUrl (..), ClientEnv,
                                           Scheme (Http), mkClientEnv,
                                           runClientM)
import           Test.Hspec
import           Test.Hspec.QuickCheck    (modifyMaxShrinks, modifyMaxSuccess)
import           Test.QuickCheck

import           Data.Hastory.API
import           Data.Hastory.Types

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
    jwk <- generateKey
    let jwtSettings = defaultJWTSettings jwk
    runNoLoggingT $
      withSqlitePoolInfo
        (mkSqliteConnectionInfo (T.pack $ fromAbsFile dbFile) & fkEnabled .~ False)
        1 $ \siPool ->
        liftIO $ do
          void $ runSqlPool (runMigrationSilent migrateAll) siPool
          let mkApp = pure $ app opts settings
              opts = Options 10 Nothing
              settings = ServerSettings siPool jwtSettings defaultCookieSettings
          testWithApplication mkApp $ \p ->
            let siClientEnv = mkClientEnv manager (BaseUrl Http "127.0.0.1" p "")
             in func (ServerInfo {..})

type RegistrationData = (UserId, Token)

withNewUser :: ClientEnv -> (RegistrationData -> Expectation) -> Expectation
withNewUser clientEnv func = do
  userForm <- randomUserForm
  Right userId <- runClientM (createUserClient  userForm) clientEnv
  Right resp <- runClientM (createSessionClient userForm) clientEnv
  case extractJWTCookie resp of
    Nothing     -> expectationFailure "JWT Cookie not found"
    Just cookie -> func (userId, Token cookie)

extractJWTCookie :: Headers AuthCookies NoContent -> Maybe B.ByteString
extractJWTCookie headerList = do
  (_, cookie) <- find jwtCookie (getHeaders headerList)
  pure . B.takeWhile (/= ';') $ B.drop 11 cookie
  where jwtCookie (headerName, headerValue) = headerName == "Set-Cookie" && B.take 11 headerValue == "JWT-Cookie="

randomUserForm :: IO UserForm
randomUserForm = generate $ UserForm <$> (mkUsername <$> arbitrary) <*> (mkPassword <$> arbitrary)
