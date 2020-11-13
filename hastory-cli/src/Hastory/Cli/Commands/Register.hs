{-# LANGUAGE RecordWildCards #-}

module Hastory.Cli.Commands.Register
  ( register,
  )
where

import Control.Monad.IO.Unlift (MonadUnliftIO, liftIO)
import Hastory.API
import Hastory.Cli.OptParse.Types
import Hastory.Data.UserForm
import System.Exit

register :: MonadUnliftIO m => RemoteStorage -> m ()
register RemoteStorage {..} = do
  client <- mkUnauthenticatedClient remoteStorageBaseUrl
  liftIO $ do
    resp <- runHastoryClient (createUserClient userForm) client
    case resp of
      Left clientError -> die $ "Register command failed. Error: " ++ show clientError
      Right _userId -> putStrLn "Registration succeeded"
  where
    userForm = UserForm remoteStorageUsername remoteStoragePassword
