{-# LANGUAGE RecordWildCards #-}

module Data.Hastory.Server.Handler.Users where

import Data.Hastory.Server.Handler.Import

createUserHandler :: ServerSettings -> UserForm -> Handler UserId
createUserHandler ServerSettings {..} userForm@UserForm {..} = do
  mUser <- runDB (getBy $ UniqueUsername userFormUserName) _ssDbPool
  let validForm = isNothing mUser || isValid userForm
  if validForm
    then throwError err400
    else buildAndInsertUser userFormUserName userFormPassword
  where
    buildAndInsertUser name password = do
      user <- User name <$> liftIO (hashPassword . mkPassword $ password)
      runDB (insert user) _ssDbPool
