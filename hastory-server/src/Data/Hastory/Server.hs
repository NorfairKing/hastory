{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}

module Data.Hastory.Server where

import           Conduit                        (MonadUnliftIO)
import           Control.Monad
import           Control.Monad.IO.Class         (MonadIO, liftIO)
import           Control.Monad.Logger           (MonadLogger)
import           Control.Monad.Logger.CallStack (logInfo)
import           Data.Semigroup                 ((<>))
import qualified Data.Text                      as T
import qualified Database.Persist.Sqlite        as SQL
import           Lens.Micro
import qualified Network.HTTP.Types             as HTTP
import qualified Network.Wai                    as Wai
import qualified Network.Wai.Handler.Warp       as Warp
import qualified Options.Applicative            as A
import           Path
import           Path.IO
import           Prelude
import           Servant                        hiding (BadPassword, NoSuchUser)
import           Servant.Auth.Server

import           Data.Hastory.API
import           Data.Hastory.Server.Handler
import           Hastory.Server.Data            (migrateAll)

data Options =
  Options
    { _oPort    :: Int
    -- ^ Port that will be used by the server.
    , _oLogFile :: Maybe String
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
server Options {..} serverSettings = createUserHandler serverSettings :<|> createSessionHandler serverSettings :<|> createEntryHandler serverSettings

-- | Main warp application. Consumes requests and produces responses.
app :: Options -> ServerSettings -> Application
app options serverSettings@ServerSettings{..} = serveWithContext api context (server options serverSettings)
  where context = _ssCookieSettings :. _ssJWTSettings :. EmptyContext

-- | Logging action that will be executed with every request.
mkWarpLogger :: FilePath -> Wai.Request -> HTTP.Status -> Maybe Integer -> IO ()
mkWarpLogger logPath req _ _ = appendFile logPath $ show req <> "\n"

-- | Warp server settings.
mkWarpSettings :: Options -> Warp.Settings
mkWarpSettings Options {..} =
  Warp.setTimeout 20 $
  Warp.setPort _oPort $ maybe id (Warp.setLogger . mkWarpLogger) _oLogFile Warp.defaultSettings

-- | Displays the port this server will use. This port is configurable via command-line flags.
reportPort :: MonadLogger m => Options -> m ()
reportPort Options {..} = logInfo $ "Starting server on port " <> T.pack (show _oPort)

-- | Starts a webserver by reading command line flags.
hastoryServer :: (MonadIO m, MonadLogger m, MonadUnliftIO m) => m ()
hastoryServer = do
  options@Options {..} <- liftIO $ A.execParser optParser
  let _ssJWTSettings = undefined
      _ssCookieSettings = defaultCookieSettings
  reportPort options
  dbFile <- resolveFile' "hastory.sqlite3"
  ensureDir (parent dbFile)
  SQL.withSqlitePoolInfo
    (SQL.mkSqliteConnectionInfo (T.pack $ fromAbsFile dbFile) & SQL.fkEnabled .~ False)
    1 $ \_ssDbPool -> do
    void $ SQL.runSqlPool (SQL.runMigrationSilent migrateAll) _ssDbPool
    liftIO $ Warp.runSettings (mkWarpSettings options) (app options ServerSettings{..})
