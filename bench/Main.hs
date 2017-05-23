module Main where

import Control.Exception
import System.Environment
import System.Exit
import System.IO.Error
import System.IO.Silently

import Criterion.Main as Criterion

import Hastory

main :: IO ()
main =
    Criterion.defaultMain
        [ bench "help" $
          whnfIO $
          runHastory ["--help"] `catch`
          (\ec ->
               pure $
               case ec of
                   ExitSuccess -> ()
                   ExitFailure _ -> ())
        ]

runHastory :: [String] -> IO ()
runHastory args = silence $ withArgs args hastory
