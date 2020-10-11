module Main where

import Control.Monad.Logger
import Prelude

import Hastory.Server

main :: IO ()
main = runStdoutLoggingT hastoryServer
