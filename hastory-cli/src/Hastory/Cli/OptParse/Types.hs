module Hastory.Cli.OptParse.Types where

import Data.Hastory.Types
import Data.Text (Text)
import Path (Abs, Dir, Path)
import Servant.Client.Core.Reexport (BaseUrl)

data Arguments =
  Arguments Command Flags
  deriving (Show)

data Instructions =
  Instructions Dispatch Settings
  deriving (Show)

data Command
  = CommandGather
  | CommandGenGatherWrapperScript GenGatherWrapperScriptFlags
  | CommandListRecentDirs ListRecentDirArgs
  | CommandChangeDir Int
  | CommandGenChangeWrapperScript
  | CommandSuggestAlias
  deriving (Show, Eq)

type GenGatherWrapperScriptFlags = Maybe RemoteStorageClientInfo

newtype ListRecentDirArgs =
  ListRecentDirArgs
    { lrdArgBypassCache :: Maybe Bool
    }
  deriving (Show, Eq)

data Flags =
  Flags
    { flagCacheDir :: Maybe FilePath
    , flagStorageServer :: Maybe Text
    , flagStorageUsername :: Maybe Username
    , flagStoragePassword :: Maybe Text
    }
  deriving (Show)

data Configuration =
  Configuration
  deriving (Show, Eq)

data Dispatch
  = DispatchGather
  | DispatchGenGatherWrapperScript GenGatherWrapperScriptSettings
  | DispatchListRecentDirs ListRecentDirSets
  | DispatchChangeDir Int
  | DispatchGenChangeWrapperScript
  | DispatchSuggestAlias
  deriving (Show, Eq)

type GenGatherWrapperScriptSettings = Maybe RemoteStorageClientInfo

newtype ListRecentDirSets =
  ListRecentDirSets
    { lrdSetBypassCache :: Bool
    }
  deriving (Show, Eq)

data Settings =
  Settings
    { setCacheDir :: Path Abs Dir
    , remoteStorageClientInfo :: Maybe RemoteStorageClientInfo
    }
  deriving (Show)

data RemoteStorageClientInfo =
  RemoteStorageClientInfo
    { remoteStorageClientInfoBaseUrl :: BaseUrl
    , remoteStorageClientInfoUsername :: Username
    , remoteStorageClientInfoPassword :: Text
    }
  deriving (Show, Eq)
