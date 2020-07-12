{-# LANGUAGE RecordWildCards #-}

module Hastory.Cli.Commands.GenGatherWrapper where

import qualified Data.Text as T
import Servant.Client.Core

import Data.Hastory
import Hastory.Cli.OptParse.Types

genGatherWrapperScript :: Maybe RemoteStorageClientInfo -> IO ()
genGatherWrapperScript = putStrLn . genScript

genScript :: Maybe RemoteStorageClientInfo -> String
genScript mRemoteStorage =
  unlines
    [ "FIRST_PROMPT=1"
    , "function hastory_gather_ {"
    , "  AT_PROMPT=1"
    , "  if [[ -n \"$FIRST_PROMPT\" ]]; then"
    , "    unset FIRST_PROMPT"
    , "    return"
    , "  fi"
    , "  echo $(fc -nl $((HISTCMD - 1))) | hastory gather" <> remoteStorageFlags
    , "}"
    ]
  where
    remoteStorageFlags =
      case mRemoteStorage of
        Nothing -> ""
        Just RemoteStorageClientInfo {..} ->
          unlines
            [ "--storage-server-username=" <>
              (T.unpack . usernameText) remoteStorageClientInfoUsername
            , "--storage-server-url=" <> showBaseUrl remoteStorageClientInfoBaseUrl
            , "--storage-server-password=" <> T.unpack remoteStorageClientInfoPassword
            ]
