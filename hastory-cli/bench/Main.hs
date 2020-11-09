{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad.Catch
import Control.Monad.Reader
import Control.Monad.State
import Criterion.Main as Criterion
import Criterion.Types as Criterion
import Data.GenValidity
import Hastory.Cli
import Hastory.Cli.Commands.Gather
import Hastory.Cli.OptParse
import Hastory.Data.Client.DB
import Hastory.Gen ()
import Import
import System.Environment
import System.Exit
import System.IO.Silently
import Test.QuickCheck
import UnliftIO (MonadUnliftIO)

main :: IO ()
main =
  let config = Criterion.defaultConfig {Criterion.reportFile = Just "bench.html"}
   in Criterion.defaultMainWith
        config
        [ bench "help"
            $ whnfIO
            $ runHastory ["--help"]
              `catch` ( \ec ->
                          pure $
                            case ec of
                              ExitSuccess -> ()
                              ExitFailure _ -> ()
                      ),
          bgroup "gather" $ map gatherBenchmark [10, 1000, 100000],
          bgroup "list-recent-directories" $ map listRecentDirsBenchmark [10, 1000, 100000]
        ]

runHastory :: [String] -> IO ()
runHastory args = silence $ withArgs args hastoryCli

gatherBenchmark :: Int -> Benchmark
gatherBenchmark entryCount =
  envWithCleanup
    (createEnv entryCount)
    cleanupEnv
    (bench ("gather-" ++ show entryCount) . whnfIO . runReaderT (gatherFrom "ls -lr"))

listRecentDirsBenchmark :: Int -> Benchmark
listRecentDirsBenchmark entryCount =
  envWithCleanup
    (createEnv entryCount)
    cleanupEnv
    ( \_settings ->
        bench ("list-recent-directories-" ++ show entryCount)
          $ whnfIO
          $ runHastory ["list-recent-directories", "--bypass-cache", "--cache-dir", "/tmp/hastory-cache"]
    )

createEnv :: Int -> IO Settings
createEnv entryCount = do
  settings <- prepareSettings
  _ <- runReaderT (prepareEntries entryCount) settings
  pure settings

cleanupEnv :: Settings -> IO ()
cleanupEnv = removeDirRecur . setCacheDir

prepareSettings :: IO Settings
prepareSettings = do
  systemTempDir <- getTempDir
  hastoryTempDir <- createTempDir systemTempDir "hastory"
  pure $ Settings hastoryTempDir hastoryTempDir

prepareEntries :: (MonadUnliftIO m, MonadReader Settings m) => Int -> m ()
prepareEntries i = do
  absDirs <- liftIO getSomeAbsDirs
  replicateM_ i $ do
    entry <-
      liftIO
        $ generate
        $ do
          t <- genValid
          d <- elements absDirs
          zt <- genValid
          u <- genValid
          pure
            Entry
              { entryText = t,
                entryWorkingDir = d,
                entryDateTime = zt,
                entryUser = u,
                entrySyncWitness = Nothing,
                entryHostName = Nothing
              }
    storeHistory entry

getSomeAbsDirs :: MonadIO m => m [Path Abs Dir]
getSomeAbsDirs = do
  home <- getHomeDir
  flip evalStateT 0 $
    walkDirAccum
      ( Just
          ( \_ _ _ -> do
              nr <- get
              pure $
                if (nr :: Int) >= 500
                  then WalkFinish
                  else WalkExclude []
          )
      )
      ( \_ ds _ -> do
          modify (+ length ds)
          pure ds
      )
      home
