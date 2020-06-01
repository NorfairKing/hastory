{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}

module Data.Hastory.Server where

import           Conduit                        (MonadUnliftIO)
import           Control.Monad
import           Control.Monad.Except
import           Control.Monad.IO.Class         (MonadIO, liftIO)
import           Control.Monad.Logger           (MonadLogger)
import           Control.Monad.Logger.CallStack (logInfo)
import           Control.Monad.Reader
import           Data.Hastory.API
import           Data.Hastory.Server.Utils
import           Data.Hastory.Types
import           Data.Pool                      (Pool)
import           Data.Proxy                     (Proxy (..))
import           Data.Semigroup                 ((<>))
import qualified Data.Text                      as T
import           Database.Persist
import           Database.Persist.Sql
import qualified Database.Persist.Sqlite        as SQL
import           Hastory.Server.Data            (migrateAll)
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

data Options =
  Options
    { _oPort    :: Int
    -- ^ Port that will be used by the server.
    , _oLogFile :: Maybe String
      -- ^ If provided, server will log to this file. If not provided, server
      -- doesn't log anything by default.
    }
  deriving (Show, Eq)

data ServerSettings =
  ServerSettings
    { _ssDbPool         :: Pool SqlBackend
      -- ^ A pool of database connections.
        --
        -- Curently, the database file is located at "~/hastory-data/hastory.db"
    , _ssJWTSettings    :: JWTSettings
    , _ssCookieSettings :: CookieSettings
    }

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

createEntryHandler :: ServerSettings -> AuthResult AuthCookie -> SyncRequest -> Handler NoContent
createEntryHandler _ BadPassword _                                 = unAuthenticated
createEntryHandler _ Indefinite _                                  = unAuthenticated
createEntryHandler _ NoSuchUser _                                  = unAuthenticated
createEntryHandler ServerSettings{..} (Authenticated cookie) syncReq = do
  user <- runDB (getBy $ UniqueUsername (unAuthCookie cookie)) _ssDbPool >>= ensureWith err401
  let serverEntry = toServerEntry syncReq (entityKey user)
      upsertIfNotExist = upsertBy uniqueContentHash serverEntry []
      uniqueContentHash = UniqueContentHash (serverEntryContentHash serverEntry)
  _ <- runDB upsertIfNotExist _ssDbPool
  pure NoContent

createUserHandler :: ServerSettings -> UserForm -> Handler UserId
createUserHandler ServerSettings{..} UserForm{..} = do
  userNameCount <- runDB (count [UserName ==. userFormUserName]) _ssDbPool
  when (userNameCount > 0) (throwError err400)
  user <- User userFormUserName <$> liftIO (hashPassword userFormPassword)
  runDB (insert user) _ssDbPool

createSessionHandler :: ServerSettings -> UserForm -> Handler (Headers AuthCookies NoContent)
createSessionHandler ServerSettings{..} UserForm{..} = do
  user <- entityVal <$> (runDB (getBy $ UniqueUsername userFormUserName) _ssDbPool >>= ensureWith err401)
  let passwordAccepted = checkPassword userFormPassword (userHashedPassword user) == PasswordCheckSuccess
  if passwordAccepted
    then setLoggedIn
    else unAuthenticated
  where setLoggedIn = do
          let cookie = AuthCookie userFormUserName
          applyCookies <- liftIO (acceptLogin _ssCookieSettings _ssJWTSettings cookie) >>= ensureWith err401
          pure $ applyCookies NoContent

-- | Proxy for Hastory API.
myApi :: Proxy HastoryAPI
myApi = Proxy

-- | Main warp application. Consumes requests and produces responses.
app :: Options -> ServerSettings -> Application
app options serverSettings@ServerSettings{..} = serveWithContext myApi context (server options serverSettings)
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

runDB :: ReaderT SqlBackend IO a -> Pool SqlBackend -> Handler a
runDB query pool = liftIO $ runSqlPool query pool

ensureWith :: MonadError e m => e -> Maybe a -> m a
ensureWith _ (Just a) = pure a
ensureWith e Nothing  = throwError e

unAuthenticated :: Handler a
unAuthenticated = throwError err401
