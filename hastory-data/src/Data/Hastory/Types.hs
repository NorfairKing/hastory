{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Data.Hastory.Types where

import Import

import Data.Aeson
import Data.Hashable (Hashable(hashWithSalt))
import Data.Function
import Data.Hashable.Time ()
import Data.Text (Text)
import Data.Time (ZonedTime, zonedTimeToUTC)

import Control.DeepSeq

data Entry = Entry
    { entryText :: !Text
    , entryWorkingDir :: Path Abs Dir
    , entryDateTime :: ZonedTime
    , entryHostName :: String
    , entryUser :: String
    } deriving (Show, Generic)

instance NFData Entry

instance Hashable Entry where
    hashWithSalt salt Entry {..} =
        salt `hashWithSalt` entryText `hashWithSalt` toFilePath entryWorkingDir `hashWithSalt`
        entryDateTime `hashWithSalt`
        entryHostName `hashWithSalt`
        entryUser

instance Eq Entry where
    e1 == e2 =
        and
            [ ((==) `on` entryText) e1 e2
            , ((==) `on` (zonedTimeToUTC . entryDateTime)) e1 e2
            , ((==) `on` entryWorkingDir) e1 e2
            , ((==) `on` entryHostName) e1 e2
            , ((==) `on` entryUser) e1 e2
            ]

instance Validity Entry where
    isValid Entry {..} = isValid entryText && isValid entryWorkingDir

instance ToJSON Entry where
    toJSON Entry {..} =
        object
            [ "t" .= entryText
            , "w" .= entryWorkingDir
            , "d" .= entryDateTime
            , "h" .= entryHostName
            , "u" .= entryUser
            ]

instance FromJSON Entry where
    parseJSON (Object o) =
        Entry <$> o .: "t" <*> o .: "w" <*> o .: "d" <*> o .: "h" <*> o .: "u"
    parseJSON _ = mempty
