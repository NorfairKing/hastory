{-# LANGUAGE RecordWildCards #-}

module Data.Hastory.Server.Handler.Users where

import Data.Hastory.Server.Handler.Import

createUserHandler :: ServerSettings -> UserForm -> Handler UserId
createUserHandler ServerSettings {..} userForm@UserForm {..} = do
  mUser <- runDB (getBy $ UniqueUsername userFormUserName) _ssDbPool
  if isNothing mUser && isValid userForm
    then buildAndInsertUser userFormUserName userFormPassword
    else throwError err400
  where
    buildAndInsertUser name password = do
      user <- User name <$> liftIO (hashPassword . mkPassword $ password)
      runDB (insert user) _ssDbPool
