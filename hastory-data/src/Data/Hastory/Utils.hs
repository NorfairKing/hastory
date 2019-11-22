{-# LANGUAGE OverloadedStrings #-}

module Data.Hastory.Utils
    ( doCountsWith
    , dataBaseSpec
    , runDb
    ) where

import Import

import Data.HashMap.Lazy (HashMap)
import qualified Data.HashMap.Lazy as HM
import Data.Hashable (Hashable)

import Data.Hastory.Types (migrateAll)
import Test.Hspec (SpecWith, Spec, ActionWith, around)
import Database.Persist.Sqlite (SqlBackend, runMigrationSilent, runSqlConn, withSqliteConn)
import Conduit (MonadUnliftIO)
import Control.Monad.Logger (runNoLoggingT)

doCountsWith ::
       (Eq b, Hashable b)
    => (a -> b)
    -> (a -> Double)
    -> [a]
    -> HashMap b Double
doCountsWith conv func = foldl go HM.empty
  where
    go hm k = HM.alter a (conv k) hm
      where
        a Nothing = Just 1
        a (Just d) = Just $ d + func k

dataBaseSpec :: SpecWith SqlBackend -> Spec
dataBaseSpec = around withDatabase

withDatabase :: ActionWith SqlBackend -> IO ()
withDatabase func = runNoLoggingT $ withSqliteConn ":memory:" $ \conn -> do
                  _ <- runDb conn (runMigrationSilent migrateAll)
                  liftIO $ func conn

runDb :: (MonadUnliftIO m) => SqlBackend -> ReaderT SqlBackend m a -> m a
runDb = flip runSqlConn
