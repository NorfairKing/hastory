{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import TestImport

import Control.Monad.Catch
import Control.Monad.Reader
import Control.Monad.State
import System.Environment
import System.IO.Silently

import Criterion.Main as Criterion
import Criterion.Types as Criterion

import Hastory
import Hastory.Gather
import Hastory.Internal
import Hastory.OptParse.Types
import Hastory.Types

import Hastory.Gen ()

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
           , bench "generate-valid-entry" $
             nfIO $ generate (genValid :: Gen Entry)
           , bgroup "gather" $ map gatherBenchmark [10, 1000, 100000]
           , bgroup "list-recent-directories" $
             map listRecentDirsBenchmark [10, 1000, 100000]
           ]

benchSets :: Settings
benchSets = Settings {setCacheDir = $(mkAbsDir "/tmp/hastory-cache")}

gatherBenchmark :: Int -> Benchmark
gatherBenchmark i =
    env
        (runReaderT (prepareEntries i) benchSets)
        (\ ~() ->
             bench ("gather-" ++ show i) $
             whnfIO $ runReaderT (gatherFrom "ls -lr") benchSets)

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
    absDirs <- liftIO getSomeAbsDirs
    replicateM_ i $ do
        entry <-
            liftIO $
            generate $ do
                t <- genValid
                d <- elements absDirs
                zt <- genValid
                s <- genValid
                u <- genValid
                pure
                    Entry
                    { entryText = t
                    , entryWorkingDir = d
                    , entryDateTime = zt
                    , entryHostName = s
                    , entryUser = u
                    }
        storeHistory entry

getSomeAbsDirs :: (MonadIO m, MonadThrow m) => m [Path Abs Dir]
getSomeAbsDirs = do
    home <- getHomeDir
    flip evalStateT 0 $
        walkDirAccum
            (Just
                 (\_ _ _ -> do
                      nr <- get
                      pure $
                          if (nr :: Int) >= 500
                              then WalkFinish
                              else WalkExclude []))
            (\_ ds _ -> do
                 modify (+ (length ds))
                 pure ds)
            home

clearCacheDir :: (MonadIO m, MonadReader Settings m) => m ()
clearCacheDir = do
    cacheDir <- hastoryDir
    liftIO $ ignoringAbsence $ removeDirRecur cacheDir

runHastory :: [String] -> IO ()
runHastory args = silence $ withArgs args hastory
