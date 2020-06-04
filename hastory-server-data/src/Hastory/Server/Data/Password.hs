{-# OPTIONS_GHC -Wno-orphans #-}

module Hastory.Server.Data.Password
  ( module X
  ) where

import Data.Password as X
import Data.Password.Bcrypt as X hiding (newSalt)
import Data.Password.Instances ()
import Data.Validity

instance Validity Password where
  validate = trivialValidation
