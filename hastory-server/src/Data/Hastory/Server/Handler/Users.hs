{-# LANGUAGE RecordWildCards #-}

module Data.Hastory.Server.Handler.Users where

import Data.Hastory.Server.Handler.Import

createUserHandler :: ServerSettings -> UserForm -> Handler UserId
createUserHandler ServerSettings {..} UserForm {..} = do
  userNameCount <- runDB (count [UserName ==. userFormUserName]) _ssDbPool
  when (userNameCount > 0) (throwError err400)
  user <- User userFormUserName <$> liftIO (hashPassword userFormPassword)
  runDB (insert user) _ssDbPool
