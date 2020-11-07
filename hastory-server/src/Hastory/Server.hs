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
import Data.Aeson (eitherDecode)
import qualified Data.ByteString.Lazy.Char8 as C
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
import System.Environment
import System.Exit

import Hastory.API
import Hastory.Data.Server.DB (migrateAll)
import Hastory.Server.Handler

data Options =
  Options
    { optPort :: Int
    -- ^ Port that will be used by the server.
    , optLogFile :: Maybe String
      -- ^ If provided, server will log to this file. If not provided, server
      -- doesn't log anything by default.
    }
  deriving (Show, Eq)

-- | Parser for the command line flags of Hastory Server.
optParser :: A.ParserInfo Options
optParser =
  A.info
    (Options <$> A.option A.auto (A.value 8080 <> A.showDefault <> A.long "port" <> A.short 'p') <*>
     A.option A.auto (A.value (Just "server.logs") <> A.long "log-output" <> A.short 'l'))
    mempty

-- | Main server logic for Hastory Server.
server :: Options -> ServerSettings -> Server HastoryAPI
server Options {..} serverSettings = userHandler :<|> sessionHandler :<|> postEntryHandler
  where
    userHandler = flip runReaderT serverSettings . createUserHandler
    sessionHandler = flip runReaderT serverSettings . createSessionHandler
    postEntryHandler =
      withAuthenticated
        (\authCookie -> flip runReaderT serverSettings . createEntryHandler authCookie)
        (\_ -> runReaderT unAuthenticated serverSettings)

-- | Main warp application. Consumes requests and produces responses.
app :: Options -> ServerSettings -> Application
app options serverSettings@ServerSettings {..} =
  serveWithContext api context (server options serverSettings)
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
  signingKey <- liftIO getSigningKey
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
    liftIO $ Warp.runSettings (mkWarpSettings options) (app options ServerSettings {..})

-- | Reads the signing key for json web tokens from the HASTORY_SERVER_JWK
-- environmental variable.
getSigningKey :: IO JWK
getSigningKey = do
  rawJwk <- lookupEnv envKey >>= ensureEnv
  ensureDecode (eitherDecode . C.pack $ rawJwk)
  where
    ensureEnv Nothing = die $ envKey <> " environmental variable not found"
    ensureEnv (Just jwk) = pure jwk
    ensureDecode (Left err) = die $ "Unable to decode JWK: " <> err
    ensureDecode (Right jwk) = pure jwk
    envKey = "HASTORY_SERVER_JWK"

withAuthenticated :: (b -> a) -> a -> AuthResult b -> a
withAuthenticated whenAuthenticated whenNotAuthenticated authRes =
  case authRes of
    Authenticated res -> whenAuthenticated res
    _ -> whenNotAuthenticated
