{-# LANGUAGE DeriveGeneric #-}
module Hastory.Types where

import           Introduction

import           Data.Aeson
import           Data.Text    (Text)

newtype Entry = Entry { unEntry :: Text }
    deriving (Show, Eq, Generic)

instance ToJSON Entry where
    toJSON (Entry text) = String text
