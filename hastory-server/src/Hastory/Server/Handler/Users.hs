{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE RecordWildCards #-}

module Hastory.Server.Handler.Users where

import qualified Data.ByteString.Lazy.Char8 as C

import Hastory.Data.PasswordDifficulty

import Hastory.Server.Handler.Import

createUserHandler :: UserForm -> HastoryHandler UserId
createUserHandler userForm@UserForm {..} = do
  mUser <- runDB . getBy $ UniqueUsername userFormUserName
  case mUser of
    Nothing -> either respondWithErr buildAndInsertUser (prettyValidate userForm)
    Just _ -> throwError err400
  where
    respondWithErr err = throwError $ err400 {errBody = C.pack err}

buildAndInsertUser :: UserForm -> HastoryHandler UserId
buildAndInsertUser UserForm {..} = do
  difficulty <- asks (unPasswordDifficulty . serverSetPwDifficulty)
  user <-
    User userFormUserName <$>
    liftIO (hashPasswordWithParams difficulty . mkPassword $ userFormPassword)
  runDB $ insert user
