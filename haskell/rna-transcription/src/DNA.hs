module DNA (toRNA) where

toRNA :: String -> Either Char String
toRNA = traverse dna_rna_map
  where
    dna_rna_map :: Char -> Either Char Char
    dna_rna_map 'G' = Right 'C'
    dna_rna_map 'C' = Right 'G'
    dna_rna_map 'T' = Right 'A'
    dna_rna_map 'A' = Right 'U'
    dna_rna_map c = Left c
