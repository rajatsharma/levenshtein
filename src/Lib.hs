module Lib
  ( levenshtein,
  )
where

import Prelude

levenshtein :: String -> String -> Int
levenshtein [] [] = 0
levenshtein [] secondWord = length secondWord
levenshtein firstWord [] = length firstWord
levenshtein firstWord secondWord
  | last firstWord == last secondWord = levenshtein [last firstWord] [head secondWord]
  | otherwise =
    minimum
      [ 1 + levenshtein [head firstWord] secondWord,
        1 + levenshtein firstWord [head secondWord],
        1 + levenshtein [head firstWord] [head secondWord]
      ]
