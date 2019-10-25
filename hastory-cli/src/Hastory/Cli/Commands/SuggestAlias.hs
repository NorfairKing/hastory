{-# LANGUAGE FlexibleContexts #-}

module Hastory.Cli.Commands.SuggestAlias where

import Import

import Data.Hastory
import Hastory.Cli.Internal
import Hastory.Cli.OptParse.Types
import Hastory.Cli.Utils (doCountsWith)

import Control.Arrow ((***))
import Data.Hashable (Hashable)
import Data.Ord (Down(..), comparing)
import Data.Text (Text)

import qualified Data.HashMap.Lazy as HM
import qualified Data.Text as T

suggest :: (MonadReader Settings m, MonadThrow m, MonadUnliftIO m) => m ()
suggest = do
    rawEnts <- getLastNDaysOfHistory 7
    let tups = take 10 $ suggestions rawEnts
    let maxlen = maximum $ map (length . show . snd) tups
        formatTup (t, x) =
            show x ++
            replicate (maxlen - length (show x) + 1) ' ' ++ T.unpack (T.strip t)
    liftIO $ forM_ tups $ putStrLn . formatTup

suggestions :: [Entry] -> [(Text, Integer)]
suggestions rawEnts = map (T.unwords *** round) tups
  where
    entries = filter (not . isCDEntry) rawEnts
    prefixes = [w | e <- entries, w <- commandPrefixes e]
    tups = aggregateSuggestions prefixes

isCDEntry :: Entry -> Bool
isCDEntry = T.isPrefixOf (T.pack "cd ") . entryText

-- tailSafe drops the empty string
commandPrefixes :: Entry -> [[Text]]
commandPrefixes = tailSafe . inits . T.words . entryText

aggregateSuggestions :: (Eq a, Hashable a) => [a] -> [(a, Double)]
aggregateSuggestions =
    sortBy (comparing $ Down . snd) . HM.toList . doCountsWith id (const 1.0)
