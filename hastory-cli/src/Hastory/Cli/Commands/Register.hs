{-# LANGUAGE RecordWildCards #-}

module Hastory.Cli.Commands.Register
  ( register,
  )
where

import Control.Monad.IO.Unlift (MonadUnliftIO, liftIO)
import Hastory.API
import Hastory.Cli.OptParse.Types
import Hastory.Data.UserForm
import Network.HTTP.Client
import Network.HTTP.Conduit (tlsManagerSettings)
import Servant.Client hiding (client, manager)
import System.Exit

register :: MonadUnliftIO m => RegisterSettings -> m ()
register (RegisterSettings RemoteStorage {..}) =
  liftIO $ do
    client <- mkClient
    resp <- runHastoryClient (createUserClient userForm) client
    case resp of
      Left clientError -> die $ "Register command failed. Error: " ++ show clientError
      Right _userId -> putStrLn "Registration succeeded"
  where
    userForm = UserForm remoteStorageUsername remoteStoragePassword
    mkClient = acceptManager <$> newManager tlsManagerSettings
    acceptManager = flip mkClientEnv remoteStorageBaseUrl
