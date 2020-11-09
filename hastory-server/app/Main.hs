module Main where

import Control.Monad.Logger
import Hastory.Server
import Prelude

main :: IO ()
main = runStdoutLoggingT hastoryServer
