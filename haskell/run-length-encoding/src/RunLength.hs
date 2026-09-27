module RunLength (decode, encode) where

import Data.Char (isDigit)

decode :: String -> String
decode "" = ""
decode encodedText = finalize (foldl step ("", "") encodedText)
  where
    step (count_string, result) element
      | not (isDigit element) && count_string == "" = ("", result ++ [element])
      | not (isDigit element) = ("", result ++ replicate (read count_string) element)
      | otherwise = (count_string ++ [element], result)

    finalize :: (String, String) -> String
    finalize (_, result) = result

encode :: String -> String
encode "" = ""
encode text = finalize (foldl step (0, head text, "") text)
  where
    step (count, letter, result) element
      | element == letter = (count + 1, letter, result)
      | otherwise = (1, element, result ++ encodeRun count letter)

    encodeRun 1 c = [c]
    encodeRun n c = show n ++ [c]

    finalize :: (Int, Char, String) -> String
    finalize (count, letter, result) = result ++ encodeRun count letter
