{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Hastory where

import Introduction

import qualified Data.Aeson as JSON
import qualified Data.ByteString as SB
import qualified Data.ByteString.Lazy as LB
import qualified Data.ByteString.Lazy.Char8 as LB8
import Data.HashMap.Lazy (HashMap)
import qualified Data.HashMap.Lazy as HM
import Data.Hashable (Hashable)
import Data.List (unwords)
import qualified Data.Text.IO as T
import qualified Data.Time.LocalTime as Time

import Hastory.OptParse
import Hastory.Types

hastory :: IO ()
hastory = do
    (d, Settings) <- getInstructions
    dispatch d

dispatch :: Dispatch -> IO ()
dispatch DispatchGather = gather
dispatch DispatchQuery = query

gather :: IO ()
gather = do
    curtime <- Time.getZonedTime
    curdir <- getCurrentDir
    text <- T.getContents
    storeHistory
        Entry
        {entryText = text, entryDateTime = curtime, entryWorkingDir = curdir}

storeHistory :: Entry -> IO ()
storeHistory entry = do
    hFile <- histfile
    ensureDir $ parent hFile
    LB.appendFile (toFilePath hFile) $ JSON.encode entry <> "\n"

histfile :: IO (Path Abs File)
histfile = do
    home <- getHomeDir
    pure $ home </> $(mkRelDir ".hastory") </> $(mkRelFile "commandhistory.log")

query :: IO ()
query = do
    hFile <- histfile
    contents <- readFile hFile
    let encodedEntries = LB8.lines contents
    let entries = catMaybes $ map JSON.decode encodedEntries
    let tups =
            sortOn snd $
            HM.toList $ docounts $ map (toFilePath . entryWorkingDir) entries
    forM_ tups $ \(p, c) -> putStrLn $ unwords [show c, p]

docounts
    :: (Eq a, Hashable a)
    => [a] -> HashMap a Int
docounts es = foldl go HM.empty es
  where
    go = flip $ HM.alter a
      where
        a Nothing = Just 1
        a (Just d) = Just $ d + 1
