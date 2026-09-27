module RunLength (decode, encode) where

import Data.Char (intToDigit, isDigit)

decode :: String -> String
decode encodedText = error "You need to implement this function."

encode :: String -> String
encode "" = ""
encode text = finalize (foldl step (0, head text, "") text)
  where
    step (count, letter, result) elem
      | elem == letter = (count + 1, letter, result)
      | otherwise = (0, elem, result ++ encodeRun count letter)

    encodeRun 1 c = [c]
    encodeRun n c = show n ++ [c]

    finalize (count, letter, result) = result ++ encodeRun count letter
