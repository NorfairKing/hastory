{-# LANGUAGE RecordWildCards #-}

module Data.Hastory.Server.Handler.Users where

import Data.Hastory.Server.Handler.Import

createUserHandler :: ServerSettings -> UserForm -> Handler UserId
createUserHandler ServerSettings {..} UserForm {..} = do
  mUser <- runDB (getBy $ UniqueUsername userFormUserName) _ssDbPool
  case mUser of
    Just _ -> throwError err400
    Nothing -> buildAndInsertUser userFormUserName userFormPassword
  where
    buildAndInsertUser name password = do
      user <- User name <$> liftIO (hashPassword . mkPassword $ password)
      runDB (insert user) _ssDbPool
