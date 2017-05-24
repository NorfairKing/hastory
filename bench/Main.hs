{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import Control.Exception
import Control.Monad.Reader
import Path
import System.Environment
import System.Exit
import System.IO.Error
import System.IO.Silently

import Criterion.Main as Criterion

import Hastory
import Hastory.Gather
import Hastory.OptParse.Types

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
        , bench "gather" $
          whnfIO $
          runReaderT
              (gatherFrom "ls -lr")
              Settings {setCacheDir = $(mkAbsDir "/tmp/hastory-cache")}
        , bench "list-recent-directories" $
          whnfIO $
          runHastory
              [ "list-recent-directories"
              , "--bypass-cache"
              , "--cache-dir"
              , "/tmp/hastory-cache"
              ]
        ]

runHastory :: [String] -> IO ()
runHastory args = silence $ withArgs args hastory
