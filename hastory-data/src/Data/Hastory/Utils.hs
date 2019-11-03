module Data.Hastory.Utils
  ( doCountsWith
  ) where

import Data.HashMap.Lazy (HashMap)
import qualified Data.HashMap.Lazy as HM
import Data.Hashable (Hashable)

doCountsWith ::
     (Eq b, Hashable b) => (a -> b) -> (a -> Double) -> [a] -> HashMap b Double
doCountsWith conv func = foldl go HM.empty
  where
    go hm k = HM.alter a (conv k) hm
      where
        a Nothing = Just 1
        a (Just d) = Just $ d + func k
