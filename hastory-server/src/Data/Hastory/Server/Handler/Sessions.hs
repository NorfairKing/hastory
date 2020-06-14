{-# LANGUAGE RecordWildCards #-}

module Data.Hastory.Server.Handler.Sessions where

import Data.Hastory.Server.Handler.Import

createSessionHandler :: UserForm -> HastoryHandler (Headers AuthCookies NoContent)
createSessionHandler UserForm {..} = do
  user <- entityVal <$> (runDB (getBy $ UniqueUsername userFormUserName) >>= ensureWith err401)
  case checkPassword (mkPassword userFormPassword) (userHashedPassword user) of
    PasswordCheckSuccess -> setLoggedIn
    PasswordCheckFail -> unAuthenticated
  where
    setLoggedIn = do
      let cookie = AuthCookie userFormUserName
      cookieSettings <- asks _ssCookieSettings
      jwtSettings <- asks _ssJWTSettings
      applyCookies <- liftIO (acceptLogin cookieSettings jwtSettings cookie) >>= ensureWith err401
      pure $ applyCookies NoContent
