{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE RecordWildCards #-}

module Data.Hastory.Server.Handler.Users where

import qualified Data.ByteString.Lazy.Char8 as C

import Data.Hastory.Server.Handler.Import

createUserHandler :: UserForm -> HastoryHandler UserId
createUserHandler userForm@UserForm {..} = do
  mUser <- runDB . getBy $ UniqueUsername userFormUserName
  case mUser of
    Nothing -> either respondWithErr buildAndInsertUser (prettyValidate userForm)
    Just _ -> throwError err400
  where
    respondWithErr err = throwError $ err400 {errBody = C.pack err}

buildAndInsertUser :: UserForm -> HastoryHandler UserId
buildAndInsertUser UserForm {..} =
  User userFormUserName <$> liftIO (hashPassword . mkPassword $ userFormPassword) >>= runDB . insert
