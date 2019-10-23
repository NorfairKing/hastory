{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Hastory.Types where

import           Data.Aeson
import           Data.Hashable      (Hashable (hashWithSalt))
import           Data.Hashable.Time ()
import           Data.Text          (Text)
import           Data.Time          (ZonedTime, zonedTimeToUTC)
import           Data.Validity
import           Data.Validity.Path ()
import           Data.Validity.Text ()
import           GHC.Generics       as X
import           Path
import           Prelude

import           Control.DeepSeq

data Entry = Entry
    { entryText       :: Text
    , entryWorkingDir :: Path Abs Dir
    , entryDateTime   :: ZonedTime
    , entryHostName   :: String
    , entryUser       :: String
    } deriving (Show, Generic)

instance NFData Entry

instance Hashable Entry where
    hashWithSalt salt Entry {..} =
        salt `hashWithSalt` entryText `hashWithSalt`
        (toFilePath entryWorkingDir) `hashWithSalt`
        entryDateTime `hashWithSalt`
        entryHostName `hashWithSalt`
        entryUser

instance Eq Entry where
    e1 == e2 =
        and
            [ entryText e1 == entryText e2
            , zonedTimeToUTC (entryDateTime e1) ==
              zonedTimeToUTC (entryDateTime e2)
            , entryWorkingDir e1 == entryWorkingDir e2
            , entryHostName e1 == entryHostName e2
            , entryUser e1 == entryUser e2
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
