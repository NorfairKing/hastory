{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import Control.Exception
import Control.Monad.Reader
import Path
import Path.IO
import System.Environment
import System.Exit
import System.IO.Silently

import Criterion.Main as Criterion
import Criterion.Types as Criterion

import Hastory
import Hastory.Gather
import Hastory.Internal
import Hastory.OptParse.Types

main :: IO ()
main =
    let config =
            Criterion.defaultConfig {Criterion.reportFile = Just "bench.html"}
    in Criterion.defaultMainWith
           config
           [ bench "help" $
             whnfIO $
             runHastory ["--help"] `catch`
             (\ec ->
                  pure $
                  case ec of
                      ExitSuccess -> ()
                      ExitFailure _ -> ())
           , bench "gather" $
             whnfIO $ runReaderT (gatherFrom "ls -lr") benchSets
           , bgroup
                 "list-recent-directories"
                 [ listRecentDirsBenchmark 0
                 , listRecentDirsBenchmark 1000
                 , listRecentDirsBenchmark 1000000
                 ]
           ]

benchSets :: Settings
benchSets = Settings {setCacheDir = $(mkAbsDir "/tmp/hastory-cache")}

listRecentDirsBenchmark :: Int -> Benchmark
listRecentDirsBenchmark i =
    env
        (runReaderT (prepareEntries i) benchSets)
        (\ ~() ->
             bench ("list-recent-directories-" ++ show i) $
             whnfIO $
             runHastory
                 [ "list-recent-directories"
                 , "--bypass-cache"
                 , "--cache-dir"
                 , "/tmp/hastory-cache"
                 ])

prepareEntries :: (MonadIO m, MonadReader Settings m) => Int -> m ()
prepareEntries i = do
    clearCacheDir
    replicateM_ i $ gatherFrom "ls -lr"

clearCacheDir :: (MonadIO m, MonadReader Settings m) => m ()
clearCacheDir = do
    cacheDir <- hastoryDir
    liftIO $ ignoringAbsence $ removeDirRecur cacheDir

runHastory :: [String] -> IO ()
runHastory args = silence $ withArgs args hastory
