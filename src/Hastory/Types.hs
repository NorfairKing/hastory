{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Hastory.Types where

import Import

import Data.Aeson
import Data.Hashable (Hashable(hashWithSalt))
import Data.Hashable.Time ()
import Data.Text (Text)
import Data.Time (ZonedTime, zonedTimeToUTC)

data Entry = Entry
    { entryText :: Text
    , entryWorkingDir :: Path Abs Dir
    , entryDateTime :: ZonedTime
    , entryHostName :: String
    } deriving (Show, Generic)

instance Hashable Entry where
    hashWithSalt salt Entry {..} =
        salt `hashWithSalt` entryText `hashWithSalt`
        (toFilePath entryWorkingDir) `hashWithSalt`
        entryDateTime `hashWithSalt`
        entryHostName

instance Eq Entry where
    e1 == e2 =
        entryText e1 == entryText e2 &&
        zonedTimeToUTC (entryDateTime e1) == zonedTimeToUTC (entryDateTime e2) &&
        entryWorkingDir e1 == entryWorkingDir e2

instance Validity Entry where
    isValid Entry {..} = isValid entryText && isValid entryWorkingDir

instance ToJSON Entry where
    toJSON Entry {..} =
        object ["t" .= entryText, "w" .= entryWorkingDir, "d" .= entryDateTime, "h" .= entryHostName]

instance FromJSON Entry where
    parseJSON (Object o) = Entry <$> o .: "t" <*> o .: "w" <*> o .: "d" <*> o .: "h"
    parseJSON _ = mempty
