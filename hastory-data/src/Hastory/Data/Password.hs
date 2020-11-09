{-# OPTIONS_GHC -Wno-orphans #-}

module Hastory.Data.Password
  ( module Data.Password,
    module Data.Password.Bcrypt,
  )
where

import Data.Password
import Data.Password.Bcrypt hiding (newSalt)
import Data.Password.Instances ()
import Data.Validity

instance Validity Password where
  validate = trivialValidation
