{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}

module Data.Hastory.Server where

import Conduit (MonadUnliftIO)
import Control.Monad
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Logger (MonadLogger)
import Control.Monad.Logger.CallStack (logInfo)
import Data.Hastory.API
import Data.Hastory.Types (EntityField (UserName), ServerEntry (..),
                           Unique (UniqueContentHash, UniqueUsername), User (..), UserForm (..),
                           mkUserPassword)
import Data.Pool (Pool)
import Data.Proxy (Proxy (..))
import Data.Semigroup ((<>))
import qualified Data.Text as T
import qualified Database.Persist as SQL
import Database.Persist.Sql (SqlBackend)
import qualified Database.Persist.Sql as SQL
import qualified Database.Persist.Sqlite as SQL
import Hastory.Server.Data (migrateAll)
import Lens.Micro
import qualified Network.HTTP.Types as HTTP
import qualified Network.Wai as Wai
import qualified Network.Wai.Handler.Warp as Warp
import qualified Options.Applicative as A
import Path
import Path.IO
import Prelude
import Servant

data Options =
  Options
    { _oPort    :: Int
    -- ^ Port that will be used by the server.
    , _oLogFile :: Maybe String
      -- ^ If provided, server will log to this file. If not provided, server
      -- doesn't log anything by default.
    }
  deriving (Show, Eq)

newtype ServerSettings =
  ServerSettings
    {
    _ssDbPool :: Pool SqlBackend
    -- ^ A pool of database connections.
      --
      -- Curently, the database file is located at "~/hastory-data/hastory.db"
    }
  deriving (Show)

-- | Parser for the command line flags of Hastory Server.
optParser :: A.ParserInfo Options
optParser =
  A.info
    (Options <$> A.option A.auto (A.value 8080 <> A.showDefault <> A.long "port" <> A.short 'p') <*>
     A.option A.auto (A.value (Just "server.logs") <> A.long "log-output" <> A.short 'l'))
    mempty

-- | Main server logic for Hastory Server.
server :: Options -> ServerSettings -> Server HastoryAPI
server Options {..} ServerSettings {..} = createUserHandler _ssDbPool

createUserHandler :: Pool SqlBackend -> UserForm -> Handler ()
createUserHandler pool UserForm{..} = do
  userNameCount <- liftIO $ SQL.runSqlPool (SQL.count [UserName SQL.==. userFormUserName]) pool
  when (userNameCount > 0) (throwError err400)
  user <- User userFormUserName <$> liftIO (mkUserPassword userFormPassword)
  liftIO $ persistUser user pool

persistUser :: (MonadUnliftIO m) => User -> Pool SqlBackend -> m ()
persistUser user = SQL.runSqlPool (SQL.insert_ user)

-- | Proxy for Hastory API.
myApi :: Proxy HastoryAPI
myApi = Proxy

-- | Main warp application. Consumes requests and produces responses.
app :: Options -> ServerSettings -> Application
app options serverSettings = serve myApi (server options serverSettings)

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
  reportPort options
  dbFile <- resolveFile' "hastory.sqlite3"
  ensureDir (parent dbFile)
  SQL.withSqlitePoolInfo
    (SQL.mkSqliteConnectionInfo (T.pack $ fromAbsFile dbFile) & SQL.fkEnabled .~ False)
    1 $ \pool -> do
    void $ SQL.runSqlPool (SQL.runMigrationSilent migrateAll) pool
    liftIO $ Warp.runSettings (mkWarpSettings options) (app options (ServerSettings pool))
