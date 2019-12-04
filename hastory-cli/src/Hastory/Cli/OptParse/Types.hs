module Hastory.Cli.OptParse.Types where

import Data.Hastory.API (Token)
import qualified Data.Text as T
import Path (Abs, Dir, Path)
import Servant.Client.Core.Reexport (BaseUrl)

data Arguments =
  Arguments Command Flags
  deriving (Show, Eq)

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
    , flagStorageServer :: Maybe T.Text
    , flagStorageToken :: Maybe T.Text
    }
  deriving (Show, Eq)

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
  deriving (Show, Eq)

data RemoteStorageClientInfo =
  RemoteStorageClientInfo BaseUrl Token
  deriving (Show, Eq)
