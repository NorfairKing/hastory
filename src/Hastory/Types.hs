{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Hastory.Types where

import           Introduction

import           Data.Aeson
import           Data.Text    (Text)
import           Data.Time    (ZonedTime)

data Entry = Entry
    { entryText     :: Text
    , entryDateTime :: ZonedTime
    } deriving (Show, Generic)

instance ToJSON Entry where
    toJSON Entry{..} = object ["t" .= entryText, "d" .= entryDateTime]

instance FromJSON Entry where
    parseJSON (Object o) = Entry
        <$> o .: "t"
        <*> o .: "d"
