{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
module Hastory where

import           Introduction

import qualified Data.Aeson           as JSON
import qualified Data.ByteString.Lazy as LB
import qualified Data.Text.IO         as T
import           System.IO

import           Hastory.OptParse
import           Hastory.Types

hastory :: IO ()
hastory = do
    (DispatchGather, Settings) <- getInstructions
    T.getContents >>= (storeHistory . Entry)

storeHistory :: Entry -> IO ()
storeHistory entry = do
    home <- getHomeDir
    let histfile = home </> $(mkRelDir ".hastory") </> $(mkRelFile "commandhistory.log")
    ensureDir $ parent histfile
    LB.appendFile (toFilePath histfile) $ JSON.encode entry <> "\n"
