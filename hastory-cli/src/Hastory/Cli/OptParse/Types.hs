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
    , flagStorageServer :: Maybe Text
    , flagStorageUsername :: Maybe Username
    , flagStoragePassword :: Maybe Text
    }
  deriving (Show, Eq)

data Configuration =
  Configuration
  deriving (Show, Eq)

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
  deriving (Show)

data RemoteStorageClientInfo =
  RemoteStorageClientInfo
    { remoteStorageClientInfoBaseUrl :: BaseUrl
    , remoteStorageClientInfoUsername :: Username
    , remoteStorageClientInfoPassword :: Text
    }
  deriving (Show, Eq)
