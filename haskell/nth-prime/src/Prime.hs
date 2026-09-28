module Prime (nth) where

import Data.List (find)
import Data.Maybe (fromJust)

nth :: Int -> Maybe Integer
nth n | n < 1 = Nothing
nth n = Just nthPrime
  where
    numbers = [2 ..]

    step acc element
      | any (\acc_element -> (mod element acc_element) == 0) acc = acc
      | otherwise = element : acc

    nthPrime =
      head
        ( fromJust
            ( find
                (\acc -> length acc == n)
                (scanl step [] numbers)
            )
        )
