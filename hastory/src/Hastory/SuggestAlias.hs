{-# LANGUAGE FlexibleContexts #-}

module Hastory.SuggestAlias where

import Import

import Control.Arrow (second)
import Data.HashMap.Lazy (HashMap)
import qualified Data.HashMap.Lazy as HM
import Data.Hashable (Hashable)
import qualified Data.Text as T
import Data.Text (Text)

import Hastory.Internal
import Hastory.OptParse.Types
import Hastory.Types

suggest :: (MonadIO m, MonadThrow m, MonadReader Settings m) => m ()
suggest = do
    tups <- suggestions
    let maxlen = maximum $ map (length . show . snd) tups
        formatTup (t, x) =
            show x ++
            replicate (maxlen - length (show x) + 1) ' ' ++ T.unpack (T.strip t)
    liftIO $ forM_ tups $ putStrLn . formatTup

suggestions ::
       (MonadIO m, MonadThrow m, MonadReader Settings m) => m [(Text, Integer)]
suggestions = do
    rawEnts <- getLastNDaysOfHistory 7
    let entries = filter (not . T.isPrefixOf (T.pack "cd ") . entryText) rawEnts
    let counts = doCountsWith entryText (const 1.0) entries
    let tups = reverse $ sortOn snd $ HM.toList counts
    return $ take 10 $ map (second round) tups
  where
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
            a Nothing = Just 0
            a (Just d) = Just $ d + func k
