module Hastory.Cli.OptParse.Types where

import Import

data Arguments =
    Arguments Command
              Flags
    deriving (Show, Eq)

data Instructions =
    Instructions Dispatch
                 Settings
    deriving (Show, Eq)

data Command
    = CommandGather
    | CommandGenGatherWrapperScript
    | CommandListRecentDirs ListRecentDirArgs
    | CommandChangeDir Int
    | CommandGenChangeWrapperScript
    | CommandSuggestAlias
    deriving (Show, Eq)

newtype ListRecentDirArgs = ListRecentDirArgs
    { lrdArgBypassCache :: Maybe Bool
    } deriving (Show, Eq)

newtype Flags = Flags
    { flagCacheDir :: Maybe FilePath
    } deriving (Show, Eq)

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

newtype ListRecentDirSets = ListRecentDirSets
    { lrdSetBypassCache :: Bool
    } deriving (Show, Eq)

newtype Settings = Settings
    { setCacheDir :: Path Abs Dir
    } deriving (Show, Eq)
