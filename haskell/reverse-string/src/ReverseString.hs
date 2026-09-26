module ReverseString (reverseString) where

reverseString :: String -> String
reverseString [] = []
reverseString str = go "" str
  where
    go :: String -> String -> String
    go acc [] = acc
    go acc (x : xs) = go (x : acc) xs
