module Main where

import Control.Monad.Logger
import Prelude

import Data.Hastory.Server

main :: IO ()
main = runStdoutLoggingT hastoryServer
