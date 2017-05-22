module Main where

import Control.Exception
import System.Environment
import System.Exit
import System.IO.Error

import Criterion.Main as Criterion

import Hastory

main :: IO ()
main =
    Criterion.defaultMain
        [ bench "help" $
          whnfIO $ do
              withArgs
                  ["--help"]
                  (hastory `catch`
                   (\ec ->
                        pure $
                        case ec of
                            ExitSuccess -> ()
                            ExitFailure _ -> ()))
        ]
