{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeOperators       #-}

module Hastory.API where

import           Data.String  (IsString, fromString)
import qualified Data.Text    as T
import           GHC.TypeLits (symbolVal)
import           Prelude
import           Servant

-- | TODO: Document
type TokenHeaderKey = "X-Token"

-- | TODO: Document
tokenHeaderKey :: IsString s => s
tokenHeaderKey = fromString $ symbolVal (Proxy @TokenHeaderKey)

-- | TODO: Document
newtype Token = Token T.Text

-- | TODO: Document
instance FromHttpApiData Token where
  parseHeader = fmap Token . parseHeader
  parseUrlPiece = fmap Token . parseUrlPiece

-- | TODO: Document
type HastoryAPI = "commands" :> "append" :> Header TokenHeaderKey Token :> ReqBody '[JSON] String :> Post '[JSON] ()
