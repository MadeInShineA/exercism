module Pangram (isPangram) where

import Data.Char (toLower)
import Data.List (delete)

isPangram :: String -> Bool
isPangram text = remaining_letters == []
  where
    letters = ['a' .. 'z']
    remaining_letters =
      foldl
        ( \acc letter ->
            let lower_letter = toLower letter
             in if lower_letter `elem` acc
                  then delete lower_letter acc
                  else acc
        )
        letters
        text
