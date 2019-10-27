{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}

module Data.Hastory.Types where

import Import

import Data.Hastory.Types.Path ()

import Control.DeepSeq
import Data.Aeson
import Data.Function
import Data.Hashable (Hashable (hashWithSalt))
import Data.Hashable.Time ()
import Data.Text (Text)
import Data.Time (UTCTime)
import Data.Validity.Time.Clock ()
import Database.Persist.TH (mkMigrate, mkPersist, persistLowerCase, share,
                            sqlSettings)

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Entry
    text Text
    workingDir (Path Abs Dir)
    dateTime UTCTime
    hostName Text
    user Text
    deriving Show Generic Eq
|]

instance NFData Entry

instance Hashable Entry where
    hashWithSalt salt Entry {..} =
        salt `hashWithSalt` entryText `hashWithSalt` toFilePath entryWorkingDir `hashWithSalt`
        entryDateTime `hashWithSalt`
        entryHostName `hashWithSalt`
        entryUser

instance Validity Entry where
    isValid Entry {..} = isValid entryText && isValid entryWorkingDir

instance ToJSON Entry
instance FromJSON Entry
