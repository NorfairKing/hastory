module Hastory.OptParse.Types where

import Introduction

type Arguments = (Command, Flags)

type Instructions = (Dispatch, Settings)

data Command
    = CommandGather
    | CommandGenGatherWrapperScript
    | CommandListRecentDirs
    | CommandChangeDir Int
    | CommandGenChangeWrapperScript
    deriving (Show, Eq)

data Flags =
    Flags
    deriving (Show, Eq)

data Configuration =
    Configuration
    deriving (Show, Eq)

data Dispatch
    = DispatchGather
    | DispatchGenGatherWrapperScript
    | DispatchListRecentDirs
    | DispatchChangeDir Int
    | DispatchGenChangeWrapperScript
    deriving (Show, Eq)

data Settings =
    Settings
    deriving (Show, Eq)
