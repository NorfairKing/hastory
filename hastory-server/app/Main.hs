module Main where

import Control.Monad.Logger
import Prelude

import HastoryServer

main :: IO ()
main = runStdoutLoggingT hastoryServer
