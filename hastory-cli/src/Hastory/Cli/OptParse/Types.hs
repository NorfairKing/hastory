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
  | CommandGenGatherWrapperScript
  | CommandListRecentDirs ListRecentDirArgs
  | CommandChangeDir Int
  | CommandGenChangeWrapperScript
  | CommandSuggestAlias
  deriving (Show, Eq)

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
  | DispatchGenGatherWrapperScript
  | DispatchListRecentDirs ListRecentDirSets
  | DispatchChangeDir Int
  | DispatchGenChangeWrapperScript
  | DispatchSuggestAlias
  deriving (Show, Eq)

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
  RemoteStorageClientInfo BaseUrl Username Text
  deriving (Show)
