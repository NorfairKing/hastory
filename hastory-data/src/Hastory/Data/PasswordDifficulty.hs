module Hastory.Data.PasswordDifficulty
  ( PasswordDifficulty,
    unPasswordDifficulty,
    mkPasswordDifficultyWithError,
    passwordDifficultyOrExit,
  )
where

import Data.Validity
import System.Exit (die)

instance Validity PasswordDifficulty where
  validate (PasswordDifficulty n) =
    mconcat
      [ declare "Is greater than or equal to 4" (n >= 4),
        declare "Is less than or equal to 31" (n <= 31)
      ]

newtype PasswordDifficulty
  = PasswordDifficulty
      { unPasswordDifficulty :: Int
      }

mkPasswordDifficultyWithError :: Int -> Either String PasswordDifficulty
mkPasswordDifficultyWithError = prettyValidate . PasswordDifficulty

passwordDifficultyOrExit :: Int -> IO PasswordDifficulty
passwordDifficultyOrExit n =
  case mkPasswordDifficultyWithError n of
    Right pwDifficulty -> pure pwDifficulty
    Left err -> die err
