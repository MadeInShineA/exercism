module Grains (square, total) where

import Data.Maybe (mapMaybe)

square :: Integer -> Maybe Integer
square n | n < 1 = Nothing
square n | n > 64 = Nothing
square 1 = Just 1
square n = fmap (* 2) (square (n - 1))

total :: Integer
total = sum (mapMaybe square [1 .. 64])
