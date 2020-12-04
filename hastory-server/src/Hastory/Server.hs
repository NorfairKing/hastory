{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Hastory.Server where

import Conduit (MonadUnliftIO)
import Control.Monad
import Control.Monad.Logger (MonadLogger, runStdoutLoggingT)
import Control.Monad.Logger.CallStack (logInfo)
import Crypto.JOSE.JWK
import Data.Maybe
import qualified Data.Text as T
import qualified Database.Persist.Sqlite as SQL
import Hastory.API
import Hastory.Data.Server.DB (migrateAll)
import Hastory.Server.Handler
import Hastory.Server.OptParse
import Lens.Micro hiding (sets)
import qualified Network.HTTP.Types as HTTP
import qualified Network.Wai as Wai
import qualified Network.Wai.Handler.Warp as Warp
import Path.IO
import Servant.Auth.Server
import Prelude

-- | Main server logic for Hastory Server.
server :: ServerSettings -> Server HastoryAPI
server serverSettings = userHandler :<|> sessionHandler :<|> postEntryHandler
  where
    userHandler = flip runReaderT serverSettings . createUserHandler
    sessionHandler = flip runReaderT serverSettings . createSessionHandler
    postEntryHandler =
      withAuthenticated
        (\authCookie -> flip runReaderT serverSettings . createEntryHandler authCookie)
        (\_ -> runReaderT unAuthenticated serverSettings)

-- | Main warp application. Consumes requests and produces responses.
app :: ServerSettings -> Application
app serverSettings@ServerSettings {..} = serveWithContext api context (server serverSettings)
  where
    context = serverSetCookieSettings :. serverSetJWTSettings :. EmptyContext

-- | Logging action that will be executed with every request.
mkWarpLogger :: FilePath -> Wai.Request -> HTTP.Status -> Maybe Integer -> IO ()
mkWarpLogger logPath req _ _ = appendFile logPath $ show req <> "\n"

-- | Warp server settings.
mkWarpSettings :: ServeSettings -> Warp.Settings
mkWarpSettings ServeSettings {..} =
  Warp.setTimeout 20
    $ Warp.setPort serveSettingsPort
    $ Warp.setLogger (mkWarpLogger $ toFilePath serveSettingsLogFile) Warp.defaultSettings

-- | Displays the port this server will use. This port is configurable via command-line flags.
reportPort :: MonadLogger m => Int -> m ()
reportPort port = logInfo $ "Starting server on port " <> T.pack (show port)

-- | Runs command by reading from the command line, environment, and config file.
hastoryServer :: IO ()
hastoryServer = do
  Instructions d sets <- getInstructions
  runReaderT (dispatch d) sets

dispatch :: MonadUnliftIO m => Dispatch -> m ()
dispatch (DispatchServe serveSets) = hastoryServe serveSets

-- | Starts a webserver by reading command line flags
hastoryServe :: MonadUnliftIO m => ServeSettings -> m ()
hastoryServe serveSettings@ServeSettings {..} =
  runStdoutLoggingT $ do
    signingKey <- liftIO (getSigningKey serveSettingsKeyFile)
    serverSetPwDifficulty <- liftIO (passwordDifficultyOrExit 10)
    let serverSetCookieSettings = defaultCookieSettings
        serverSetJWTSettings = defaultJWTSettings signingKey
    reportPort serveSettingsPort
    dbFile <- resolveFile' "hastory.sqlite3"
    ensureDir (parent dbFile)
    SQL.withSqlitePoolInfo
      (SQL.mkSqliteConnectionInfo (T.pack $ fromAbsFile dbFile) & SQL.fkEnabled .~ False)
      1
      $ \serverSetPool -> do
        void $ SQL.runSqlPool (SQL.runMigrationSilent migrateAll) serverSetPool
        liftIO $ Warp.runSettings (mkWarpSettings serveSettings) (app ServerSettings {..})

-- | Reads the signing key from the given file. If the file does not exist, then
-- the file, with a JWK, will be created and read from.
getSigningKey :: Path Abs File -> IO JWK
getSigningKey keyPath = do
  ensureDir (parent keyPath)
  fileExists <- doesFileExist keyPath
  unless fileExists (writeKey path)
  readKey path
  where
    path = toFilePath keyPath

withAuthenticated :: (b -> a) -> a -> AuthResult b -> a
withAuthenticated whenAuthenticated whenNotAuthenticated authRes =
  case authRes of
    Authenticated res -> whenAuthenticated res
    _ -> whenNotAuthenticated
