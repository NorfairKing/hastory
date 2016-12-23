{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Hastory.Types where

import           Introduction

import           Data.Aeson
import           Data.Text    (Text)
import           Data.Time    (ZonedTime, zonedTimeToUTC)

data Entry = Entry
    { entryText     :: Text
    , entryDateTime :: ZonedTime
    } deriving (Show, Generic)

instance Eq Entry where
    e1 == e2 = entryText e1 == entryText e2 && zonedTimeToUTC (entryDateTime e1) == zonedTimeToUTC (entryDateTime e2)

instance Validity Entry where
    isValid Entry{..} = isValid entryText

instance ToJSON Entry where
    toJSON Entry{..} = object ["t" .= entryText, "d" .= entryDateTime]

instance FromJSON Entry where
    parseJSON (Object o) = Entry
        <$> o .: "t"
        <*> o .: "d"
    parseJSON _ = mempty
