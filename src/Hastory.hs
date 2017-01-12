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
import qualified Data.Time.Clock as Time
import qualified Data.Time.LocalTime as Time

import Hastory.OptParse
import Hastory.Types

hastory :: IO ()
hastory = do
    (d, Settings) <- getInstructions
    dispatch d

dispatch :: Dispatch -> IO ()
dispatch DispatchGather = gather
dispatch (DispatchChangeDir ix) = change ix
dispatch DispatchListRecentDirs = listRecentDirs
dispatch DispatchGenChangeWrapperScript = genChangeWrapperScript

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

getHistory :: IO [Entry]
getHistory = do
    hFile <- histfile
    contents <- readFile hFile
    let encodedEntries = LB8.lines contents
    let entries = catMaybes $ map JSON.decode encodedEntries
    pure entries

docounts
    :: (Eq a, Eq b, Hashable a, Hashable b)
    => (a -> b) -> [a] -> HashMap b Double
docounts f = doCountsWith f (const 1)

doCountsWith
    :: (Eq a, Eq b, Hashable a, Hashable b)
    => (a -> b) -> (a -> Double) -> [a] -> HashMap b Double
doCountsWith conv func es = foldl go HM.empty es
  where
    go hm k = HM.alter a (conv k) hm
      where
        a Nothing = Just 0
        a (Just d) = Just $ d + func k

getRecentDirOpts :: IO [FilePath]
getRecentDirOpts = do
    rawEnts <- getHistory
    home <- getHomeDir
    let entries = filter ((/= home) . entryWorkingDir) rawEnts
    now <- Time.getZonedTime
    let dateFunc entry = 1 / d
          where
            d =
                realToFrac $
                Time.diffUTCTime
                    (Time.zonedTimeToUTC now)
                    (Time.zonedTimeToUTC $ entryDateTime entry)
    let counts = doCountsWith (toFilePath . entryWorkingDir) dateFunc entries
    let tups = reverse $ sortOn snd $ HM.toList counts
    pure $ take 10 $ map fst tups

change :: Int -> IO ()
change ix = do
    recentDirOpts <- getRecentDirOpts
    case recentDirOpts `atMay` ix of
        Nothing -> die "Invalid index choice."
        Just d -> putStrLn d

listRecentDirs :: IO ()
listRecentDirs = do
    recentDirOpts <- getRecentDirOpts
    forM_ (zip [0 ..] recentDirOpts) $ \(ix, p) ->
        putStrLn $ unwords [show ix, p]

genChangeWrapperScript :: IO ()
genChangeWrapperScript =
    putStrLn $
    unlines
        [ "hastory_change_directory_ () {"
        , "  local args=\"$@\""
        , "  if [[ \"$args\" == \"\" ]]"
        , "  then"
        , "    hastory list-recent-directories"
        , "  else"
        , "    local dir=$(hastory change-directory \"$args\")"
        , "    cd dir"
        , "  fi"
        , "}"
        ]
