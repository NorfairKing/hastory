{-# LANGUAGE RecordWildCards #-}

module Data.Hastory.Server.Handler.Sessions where

import Data.Hastory.Server.Handler.Import

createSessionHandler :: UserForm -> HastoryHandler (Headers AuthCookies NoContent)
createSessionHandler UserForm {..} = do
  user <- entityVal <$> (runDB (getBy $ UniqueUsername userFormUserName) >>= ensureWithUnauthorized)
  case checkPassword (mkPassword userFormPassword) (userHashedPassword user) of
    PasswordCheckSuccess -> setLoggedIn
    PasswordCheckFail -> unAuthenticated
  where
    setLoggedIn = do
      let cookie = AuthCookie userFormUserName
      cookieSettings <- asks serverSetCookieSettings
      jwtSettings <- asks serverSetJWTSettings
      setCookie <-
        liftIO (makeSessionCookieBS cookieSettings jwtSettings cookie) >>= ensureWithUnauthorized
      pure $ addHeader (decodeUtf8 setCookie) NoContent
