{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Hastory.Cli.Commands.Gather where

import Import

import qualified Data.Aeson as JSON
import qualified Data.ByteString.Lazy as LB
import Data.Text (Text)
import Data.Text (unpack)
import Data.Text (length)
import qualified Data.Text.IO as T
import qualified Data.Text.Lazy.IO as LazyT
import qualified Data.Text.Lazy
import Data.Hastory

import Hastory.Cli.Internal
import Hastory.Cli.OptParse.Types

import Debug.Trace
import qualified Data.ByteString as B
import Control.DeepSeq
import qualified Data.ByteString as S
import System.IO

gather2 :: String -> IO ()
gather2 input = putStrLn input

gather :: (MonadIO m, MonadThrow m, MonadReader Settings m) => m ()
gather = do
    text <- liftIO T.getContents
    --text <- liftIO LazyT.getContents
    --contents <- liftIO LazyT.getContents
    --text <- liftIO LazyT.getContents
    --Data.Text.length text `seq` return ()
    --text2 <- liftIO T.getContents
    gatherFrom text
    --let str = unpack text in
    --  trace("text: " ++ str) $ gatherFrom text


gatherFrom :: (MonadIO m, MonadThrow m, MonadReader Settings m) => Text -> m ()
gatherFrom text = do
    entry <- liftIO $ gatherEntryWith text
    storeHistory entry

storeHistory ::
       (MonadIO m,  MonadReader Settings m) => Entry -> m ()
storeHistory entry = do
    hFile <- histFileFor $ entryDateTime entry
    liftIO $ do
        ensureDir $ parent hFile
        --trace("Entry: " ++ show entry) $ LB.appendFile (toFilePath hFile) $ JSON.encode entry <> "\n"
        LB.appendFile (toFilePath hFile) $ JSON.encode entry <> "\n"