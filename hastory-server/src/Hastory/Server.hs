{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Hastory.Server where

import Conduit (MonadUnliftIO)
import Control.Monad
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Logger (MonadLogger)
import Control.Monad.Logger.CallStack (logInfo)
import Crypto.JOSE.JWK
import Data.Maybe
import Data.Semigroup ((<>))
import qualified Data.Text as T
import qualified Database.Persist.Sqlite as SQL
import Lens.Micro
import qualified Network.HTTP.Types as HTTP
import qualified Network.Wai as Wai
import qualified Network.Wai.Handler.Warp as Warp
import qualified Options.Applicative as A
import Path
import Path.IO
import Prelude
import Servant hiding (BadPassword, NoSuchUser)
import Servant.Auth.Server

import Hastory.API
import Hastory.Data.Server.DB (migrateAll)
import Hastory.Server.Handler

data Options =
  Options
    { optPort :: Int
    -- ^ Port that will be used by the server; defaults to 8080.
    , optLogFile :: Maybe FilePath
    -- ^ If provided, server will log to this file. If not provided, server
    -- will log to "server.logs".
    , optKeyFile :: Maybe FilePath
    -- ^ If provided, server will read / write JWK key to this file. Default is
    -- "hastory.key".
    }
  deriving (Show, Eq)

-- | Parser for the command line flags of Hastory Server.
optParser :: A.ParserInfo Options
optParser =
  A.info
    (Options <$> A.option A.auto (A.value 8080 <> A.showDefault <> A.long "port" <> A.short 'p') <*>
     A.option A.auto (A.value (Just "server.logs") <> A.long "log-output" <> A.short 'l') <*>
     A.option A.auto (A.value (Just "hastory.key") <> A.long "hastory-key" <> A.short 'k'))
    mempty

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
mkWarpSettings :: Options -> Warp.Settings
mkWarpSettings Options {..} =
  Warp.setTimeout 20 $
  Warp.setPort optPort $ maybe id (Warp.setLogger . mkWarpLogger) optLogFile Warp.defaultSettings

-- | Displays the port this server will use. This port is configurable via command-line flags.
reportPort :: MonadLogger m => Options -> m ()
reportPort Options {..} = logInfo $ "Starting server on port " <> T.pack (show optPort)

-- | Starts a webserver by reading command line flags and the HASTORY_SERVER_JWK
-- environmental variable.
hastoryServer :: (MonadIO m, MonadLogger m, MonadUnliftIO m) => m ()
hastoryServer = do
  options@Options {..} <- liftIO $ A.execParser optParser
  keyFile <- resolveFile' (fromMaybe "hastory.key" optKeyFile)
  signingKey <- liftIO (getSigningKey keyFile)
  serverSetPwDifficulty <- liftIO (passwordDifficultyOrExit 10)
  let serverSetCookieSettings = defaultCookieSettings
      serverSetJWTSettings = defaultJWTSettings signingKey
  reportPort options
  dbFile <- resolveFile' "hastory.sqlite3"
  ensureDir (parent dbFile)
  SQL.withSqlitePoolInfo
    (SQL.mkSqliteConnectionInfo (T.pack $ fromAbsFile dbFile) & SQL.fkEnabled .~ False)
    1 $ \serverSetPool -> do
    void $ SQL.runSqlPool (SQL.runMigrationSilent migrateAll) serverSetPool
    liftIO $ Warp.runSettings (mkWarpSettings options) (app ServerSettings {..})

-- | Reads the signing key from the given file. If the file does not exist, then
-- the file, with a JWK, will be created and read from.
getSigningKey :: Path Abs File -> IO JWK
getSigningKey keyPath = do
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
