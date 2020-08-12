{-# LANGUAGE OverloadedStrings #-}

module Hastory.Cli.OptParse.Types where

import Data.Aeson
import Data.Hastory.Types
import Data.Text (Text)
import Path (Abs, Dir, Path)
import Servant.Client.Core.Reexport (BaseUrl, parseBaseUrl)
import YamlParse.Applicative

data Arguments =
  Arguments Command Flags
  deriving (Show)

data Instructions =
  Instructions Dispatch Settings
  deriving (Show)

data Command
  = CommandGather GatherFlags
  | CommandGenGatherWrapperScript GenGatherWrapperScriptFlags
  | CommandListRecentDirs ListRecentDirFlags
  | CommandChangeDir ChangeDirFlags
  | CommandGenChangeWrapperScript GenChangeWrapperScriptFlags
  | CommandSuggestAlias SuggestAliasFlags
  deriving (Show, Eq)

data GatherFlags =
  GatherFlags
  deriving (Show, Eq)

data GenGatherWrapperScriptFlags =
  GenGatherWrapperScriptFlags
  deriving (Show, Eq)

newtype ChangeDirFlags =
  ChangeDirFlags
    { changeDirFlagsIdx :: Int
    }
  deriving (Show, Eq)

data GenChangeWrapperScriptFlags =
  GenChangeWrapperScriptFlags
  deriving (Show, Eq)

data SuggestAliasFlags =
  SuggestAliasFlags
  deriving (Show, Eq)

newtype ListRecentDirFlags =
  ListRecentDirFlags
    { lrdArgBypassCache :: Maybe Bool
    }
  deriving (Show, Eq)

data Flags =
  Flags
    { flagCacheDir :: Maybe FilePath
    , flagConfigFile :: Maybe FilePath
    , flagStorageServer :: Maybe BaseUrl
    , flagStorageUsername :: Maybe Username
    , flagStoragePassword :: Maybe Text
    }
  deriving (Show, Eq)

data Environment =
  Environment
    { envCacheDir :: Maybe FilePath
    , envConfigFile :: Maybe FilePath
    , envStorageServer :: Maybe BaseUrl
    , envStorageUsername :: Maybe Username
    , envStoragePassword :: Maybe Text
    , envLrdBypassCache :: Maybe Bool
    }
  deriving (Show, Eq)

data Configuration =
  Configuration
    { configCacheDir :: Maybe FilePath
    , configStorageServer :: Maybe BaseUrl
    , configStorageUsername :: Maybe Username
    , configStoragePassword :: Maybe Text
    , configLrdBypassCache :: Maybe Bool
    }
  deriving (Show, Eq)

instance YamlSchema Configuration where
  yamlSchema = parseObject
    where
      parseObject =
        objectParser "Configuration" $
        Configuration <$> optionalField "cache-dir" "the cache directory for hastory" <*>
        eitherParser
          (propogatingParseError parseBaseUrl "Unable to parse url")
          (optionalField "url" "URL of the central storage server") <*>
        eitherParser
          (propogatingParseError parseUsername "Unable to parse username")
          (optionalField "username" "Username for the central storage server") <*>
        optionalField "password" "Password for the central storage server" <*>
        eitherParser
          (propogatingParseError parseLrdBypassCache "Unable to parse lrd-bypass-cache")
          (optionalField
             "lrd-bypass-cache"
             "Whether to recompute the recent directory options or use a cache when available")
      propogatingParseError :: (a -> Maybe b) -> String -> Maybe a -> Either String (Maybe b)
      propogatingParseError scalarParser errMsg mScalar =
        case mScalar of
          Nothing -> Right Nothing
          Just scalar -> maybe (Left errMsg) (Right . Just) (scalarParser scalar)
      parseLrdBypassCache :: String -> Maybe Bool
      parseLrdBypassCache str
        | str == "bypass-cache" = Just True
        | str == "no-bypass-cache" = Just False
        | otherwise = Nothing

instance FromJSON Configuration where
  parseJSON = viaYamlSchema

data Dispatch
  = DispatchGather GatherSettings
  | DispatchGenGatherWrapperScript GenGatherWrapperScriptSettings
  | DispatchListRecentDirs ListRecentDirSettings
  | DispatchChangeDir ChangeDirSettings
  | DispatchGenChangeWrapperScript GenChangeWrapperScriptSettings
  | DispatchSuggestAlias SuggestAliasSettings
  deriving (Show, Eq)

data GatherSettings =
  GatherSettings
  deriving (Show, Eq)

newtype GenGatherWrapperScriptSettings =
  GenGatherWrapperScriptSettings
    { genGatherWrapperScriptSetRemoteInfo :: Maybe RemoteStorageClientInfo
    }
  deriving (Show, Eq)

newtype ListRecentDirSettings =
  ListRecentDirSettings
    { lrdSetBypassCache :: Bool
    }
  deriving (Show, Eq)

newtype ChangeDirSettings =
  ChangeDirSettings
    { changeDirSetIdx :: Int
    }
  deriving (Show, Eq)

data GenChangeWrapperScriptSettings =
  GenChangeWrapperScriptSettings
  deriving (Show, Eq)

data SuggestAliasSettings =
  SuggestAliasSettings
  deriving (Show, Eq)

data Settings =
  Settings
    { setCacheDir :: Path Abs Dir
    , remoteStorageClientInfo :: Maybe RemoteStorageClientInfo
    }
  deriving (Show, Eq)

data RemoteStorageClientInfo =
  RemoteStorageClientInfo
    { remoteStorageClientInfoBaseUrl :: BaseUrl
    , remoteStorageClientInfoUsername :: Username
    , remoteStorageClientInfoPassword :: Text
    }
  deriving (Show, Eq)
