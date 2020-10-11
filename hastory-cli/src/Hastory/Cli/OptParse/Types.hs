{-# LANGUAGE OverloadedStrings #-}

module Hastory.Cli.OptParse.Types where

import Data.Aeson
import Data.Text (Text)
import Hastory.Data
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
  | CommandSync SyncFlags
  deriving (Show, Eq)

data SyncFlags =
  SyncFlags
    { syncFlagsStorageServer :: Maybe BaseUrl
    , syncFlagsUsername :: Maybe Username
    , syncFlagsPassword :: Maybe Text
    }
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
        optionalFieldWith
          "url"
          "URL of the central storage server"
          (maybeParser parseBaseUrl yamlSchema) <*>
        optionalFieldWith
          "username"
          "Username for the central storage server"
          (maybeParser parseUsername yamlSchema) <*>
        optionalField "password" "Password for the central storage server" <*>
        optionalField
          "bypass-cache"
          "Whether to recompute the recent directory options or use a cache when available"

instance FromJSON Configuration where
  parseJSON = viaYamlSchema

data Dispatch
  = DispatchGather GatherSettings
  | DispatchGenGatherWrapperScript GenGatherWrapperScriptSettings
  | DispatchListRecentDirs ListRecentDirSettings
  | DispatchChangeDir ChangeDirSettings
  | DispatchGenChangeWrapperScript GenChangeWrapperScriptSettings
  | DispatchSuggestAlias SuggestAliasSettings
  | DispatchSync SyncSettings
  deriving (Show, Eq)

data GatherSettings =
  GatherSettings
  deriving (Show, Eq)

data GenGatherWrapperScriptSettings =
  GenGatherWrapperScriptSettings
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

newtype SyncSettings =
  SyncSettings
    { syncSettingsRemoteStorage :: RemoteStorage
    }
  deriving (Show, Eq)

newtype Settings =
  Settings
    { setCacheDir :: Path Abs Dir
    }
  deriving (Show, Eq)

data RemoteStorage =
  RemoteStorage
    { remoteStorageBaseUrl :: BaseUrl
    , remoteStorageUsername :: Username
    , remoteStoragePassword :: Text
    }
  deriving (Show, Eq)
