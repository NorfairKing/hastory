module Hastory.OptParse.Types where

import Import

type Arguments = (Command, Flags)

type Instructions = (Dispatch, Settings)

data Command
    = CommandGather
    | CommandGenGatherWrapperScript
    | CommandListRecentDirs ListRecentDirArgs
    | CommandChangeDir Int
    | CommandGenChangeWrapperScript
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
    deriving (Show, Eq)

newtype ListRecentDirSets = ListRecentDirSets
    { lrdSetBypassCache :: Bool
    } deriving (Show, Eq)

newtype Settings = Settings
    { setCacheDir :: Path Abs Dir
    } deriving (Show, Eq)
