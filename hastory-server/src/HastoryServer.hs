{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}

-- | TODO: Introduce mtl with logging
-- | TODO: Drop unnecessary dependencies in cabalfiles
module HastoryServer where

import           Control.Monad.IO.Class   (liftIO)
import           Data.Proxy               (Proxy (..))
import           Data.Semigroup           ((<>))
import qualified Data.Text                as T
import           Hastory.API
import qualified Network.HTTP.Types       as HTTP
import qualified Network.Wai              as Wai
import qualified Network.Wai.Handler.Warp as Warp
import qualified Options.Applicative      as A
import           Prelude
import           Servant
import           System.Posix.Files       (fileExist)
import           System.Random            (newStdGen, randomRs)

-- | TODO: Document
data Options = Options
  { _oPort               :: Int
  , _oDataOutputFilePath :: FilePath
  , _oLogFile            :: Maybe String
  } deriving (Show, Eq)

-- | TODO: Document
newtype ServerSettings = ServerSettings
  { _ssToken :: T.Text
  } deriving (Show, Eq)

-- | TODO: Document
optParser :: A.ParserInfo Options
optParser =
  A.info (
    Options
      <$> A.option A.auto (A.value 8080 <> A.showDefault <> A.long "port" <> A.short 'p')
      <*> A.strOption (A.value "/home/yigit/.hastory/data" <> A.long "data-directory" <> A.short 'd')
      <*> A.option A.auto (A.value (Just "server.logs") <> A.long "log-output" <> A.short 'l')
  ) mempty

-- | TODO: Document
server :: Options -> ServerSettings -> Server HastoryAPI
server Options {..} ServerSettings {..} = appendCommand
  where
    appendCommand :: Maybe Token -> String -> Handler ()
    appendCommand (Just (Token token)) command
      | token == _ssToken = do
        liftIO $ putStrLn command
        liftIO $ appendFile _oDataOutputFilePath (command <> "\n")
      | otherwise =
        throwError $ err403 { errBody = "Invalid Token provided." }
    appendCommand Nothing _ =
      throwError $ err403 { errBody = tokenHeaderKey <> " header should exist." }

-- | TODO: Document
myApi :: Proxy HastoryAPI
myApi = Proxy

-- | TODO: Document
app :: Options -> ServerSettings -> Application
app options serverSettings = serve myApi (server options serverSettings)

-- | TODO: We probably shoudln't read request body here, but it can be helpful for logging.
mkWarpLogger :: FilePath -> Wai.Request -> HTTP.Status -> Maybe Integer -> IO ()
mkWarpLogger logPath req _ _ =
  appendFile logPath $ show req <> "\n"

-- | TODO: Document
mkWarpSettings :: Options -> Warp.Settings
mkWarpSettings Options {..} =
    Warp.setTimeout 20 $
    Warp.setPort _oPort $
    maybe id (Warp.setLogger . mkWarpLogger) _oLogFile
    Warp.defaultSettings

-- | TODO: Document
tokenLength :: Int
tokenLength = 20

-- | TODO: Document
generateToken :: IO T.Text
generateToken = T.pack . take tokenLength . randomRs ('a', 'z') <$> newStdGen

-- | TODO: Document
reportDataFileStatus :: Options -> IO ()
reportDataFileStatus Options {..} = do
  dataFileExists <- fileExist _oDataOutputFilePath
  if dataFileExists
    then putStrLn "Data file exists. Appending commands to it."
    else putStrLn "Data file doesn't exist. Creating a new one."

-- | TODO: Document
hastoryServer :: IO ()
hastoryServer = do
  options@Options {..} <- A.execParser optParser

  reportDataFileStatus options

  token <- generateToken
  putStrLn $ "Token: " <> T.unpack token

  Warp.runSettings (mkWarpSettings options) (app options (ServerSettings token))
