{-# LANGUAGE RecordWildCards #-}

module Data.Hastory.Server.Handler.Sessions where

import Data.Hastory.Server.Handler.Import

createSessionHandler :: UserForm -> HastoryHandler (Headers AuthCookies NoContent)
createSessionHandler UserForm {..} =
  withUser userFormUserName $ \(Entity _ user) ->
    case checkPassword (mkPassword userFormPassword) (userHashedPassword user) of
      PasswordCheckSuccess -> setLoggedIn
      PasswordCheckFail -> unAuthenticated
  where
    setLoggedIn =
      withSetCookie userFormUserName $ \setCookie ->
        pure $ addHeader (decodeUtf8 setCookie) NoContent
