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
    setLoggedIn = do
      let cookie = AuthCookie userFormUserName
          unAuthorized = throwError err401
          addCookieToHeader setCookie = pure $ addHeader (decodeUtf8 setCookie) NoContent
      cookieSettings <- asks serverSetCookieSettings
      jwtSettings <- asks serverSetJWTSettings
      mSetCookie <- liftIO (makeSessionCookieBS cookieSettings jwtSettings cookie)
      maybe unAuthorized addCookieToHeader mSetCookie
