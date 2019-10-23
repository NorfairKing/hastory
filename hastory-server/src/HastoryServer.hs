{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeOperators       #-}

module HastoryServer where

import           Control.Monad.IO.Class   (liftIO)
import           Data.Proxy               (Proxy (..))
import           Data.Semigroup           ((<>))
import           Data.String              (IsString, fromString)
import qualified Data.Text                as T
import           GHC.TypeLits
import qualified Network.HTTP.Types       as HTTP
import qualified Network.Wai              as Wai
import qualified Network.Wai.Handler.Warp as Warp
import qualified Options.Applicative      as A
import           Prelude
import           Servant
import           System.Posix.Files       (fileExist)
import           System.Random            (newStdGen, randomRs)

newtype Token = Token T.Text

instance FromHttpApiData Token where
  parseHeader = fmap Token . parseHeader
  parseUrlPiece = fmap Token . parseUrlPiece

type HastoryAPI = "commands" :> "append" :> Header TokenHeaderKey Token :> ReqBody '[JSON] String :> Post '[JSON] ()

data Options = Options
  { _oPort               :: Int
  , _oDataOutputFilePath :: FilePath
  , _oLogFile            :: Maybe String
  } deriving (Show, Eq)

newtype ServerSettings = ServerSettings
  { _ssToken :: T.Text
  } deriving (Show, Eq)

type TokenHeaderKey = "X-Token"

tokenHeaderKey :: IsString s => s
tokenHeaderKey = fromString $ symbolVal (Proxy @TokenHeaderKey)

optParser :: A.ParserInfo Options
optParser =
  A.info (
    Options
      <$> A.option A.auto (A.value 8080 <> A.showDefault <> A.long "port" <> A.short 'p')
      <*> A.strOption (A.value "/home/yigit/.hastory/data" <> A.long "data-directory" <> A.short 'd')
      <*> A.option A.auto (A.value (Just "server.logs") <> A.long "log-output" <> A.short 'l')
  ) mempty

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

myApi :: Proxy HastoryAPI
myApi = Proxy

app :: Options -> ServerSettings -> Application
app options serverSettings = serve myApi (server options serverSettings)

-- | TODO: We probably shoudln't read request body here, but it can be helpful for logging.
mkWarpLogger :: FilePath -> Wai.Request -> HTTP.Status -> Maybe Integer -> IO ()
mkWarpLogger logPath req _ _ =
  appendFile logPath $ show req <> "\n"

mkWarpSettings :: Options -> Warp.Settings
mkWarpSettings Options {..} =
    Warp.setTimeout 20 $
    Warp.setPort _oPort $
    maybe id (Warp.setLogger . mkWarpLogger) _oLogFile
    Warp.defaultSettings

tokenLength :: Int
tokenLength = 20

generateToken :: IO T.Text
generateToken = T.pack . take tokenLength . randomRs ('a', 'z') <$> newStdGen

reportDataFileStatus :: Options -> IO ()
reportDataFileStatus Options {..} = do
  dataFileExists <- fileExist _oDataOutputFilePath
  if dataFileExists
    then putStrLn "Data file exists. Appending commands to it."
    else putStrLn "Data file doesn't exist. Creating a new one."

hastoryServer :: IO ()
hastoryServer = do
  options@Options {..} <- A.execParser optParser

  reportDataFileStatus options

  token <- generateToken
  putStrLn $ "Token: " <> T.unpack token

  Warp.runSettings (mkWarpSettings options) (app options (ServerSettings token))
