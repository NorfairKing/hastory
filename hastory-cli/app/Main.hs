module Main where

import Hastory.Cli
import System.IO

main :: IO ()
main = do
  --hSetBuffering stdin LineBuffering
  --hSetBuffering stdout LineBuffering
  hastoryCli
