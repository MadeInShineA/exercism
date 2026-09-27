module DNA (nucleotideCounts, Nucleotide (..)) where

import Data.Map (Map)
import Data.Map.Strict qualified as Map

data Nucleotide = A | C | G | T deriving (Eq, Ord, Show)

nucleotideCounts :: String -> Either String (Map Nucleotide Int)
nucleotideCounts string = fmap count (traverse toNucleotide string)
  where
    toNucleotide :: Char -> Either String Nucleotide
    toNucleotide 'A' = Right A
    toNucleotide 'C' = Right C
    toNucleotide 'G' = Right G
    toNucleotide 'T' = Right T
    toNucleotide c = Left ("invalid nucleotide: " ++ [c])

    count :: [Nucleotide] -> Map Nucleotide Int
    count = foldl (\acc element -> Map.insertWith (+) element 1 acc) zeroes
      where
        zeroes = Map.fromList [(n, 0) | n <- [A, C, G, T]]
