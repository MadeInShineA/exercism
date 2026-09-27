module SumOfMultiples (sumOfMultiples) where

import Data.List (nub)

sumOfMultiples :: [Integer] -> Integer -> Integer
sumOfMultiples factors limit = sum (nub (concat list_of_elements))
  where
    list_of_elements = map (\factor -> [factor, 2 * factor .. limit - 1]) (filter (/= 0) factors)
