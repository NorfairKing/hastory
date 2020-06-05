{-# LANGUAGE RecordWildCards #-}

module Data.Hastory.Server.Handler.Sessions where

import Data.Hastory.Server.Handler.Import

createSessionHandler :: ServerSettings -> UserForm -> Handler (Headers AuthCookies NoContent)
createSessionHandler ServerSettings {..} UserForm {..} = do
  user <-
    entityVal <$> (runDB (getBy $ UniqueUsername userFormUserName) _ssDbPool >>= ensureWith err401)
  case checkPassword (mkPassword userFormPassword) (userHashedPassword user) of
    PasswordCheckSuccess -> setLoggedIn
    PasswordCheckFail -> unAuthenticated
  where
    setLoggedIn = do
      let cookie = AuthCookie userFormUserName
      applyCookies <-
        liftIO (acceptLogin _ssCookieSettings _ssJWTSettings cookie) >>= ensureWith err401
      pure $ applyCookies NoContent
