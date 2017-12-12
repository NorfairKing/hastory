{-# LANGUAGE FlexibleContexts #-}

module Hastory.SuggestAlias where

import Import

import Control.Arrow (second)
import qualified Data.HashMap.Lazy as HM
import qualified Data.Text as T
import Data.Text (Text)

import Hastory.Internal
import Hastory.OptParse.Types
import Hastory.Types
import Hastory.Utils (doCountsWith)

suggest :: (MonadIO m, MonadThrow m, MonadReader Settings m) => m ()
suggest = do
    rawEnts <- getLastNDaysOfHistory 7
    let tups = suggestions rawEnts
    let maxlen = maximum $ map (length . show . snd) tups
        formatTup (t, x) =
            show x ++
            replicate (maxlen - length (show x) + 1) ' ' ++ T.unpack (T.strip t)
    liftIO $ forM_ tups $ putStrLn . formatTup

suggestions :: [Entry] -> [(Text, Integer)]
suggestions rawEnts = take 10 $ map (second round) tups
    where
        entries = filter (not . T.isPrefixOf (T.pack "cd ") . entryText) rawEnts
        counts = doCountsWith entryText (const 1.0) entries
        tups = reverse $ sortOn snd $ HM.toList counts
