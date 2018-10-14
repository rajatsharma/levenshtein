module Data.Lavenshtein where

import Prelude

import Data.Array (last, init, foldl)
import Data.Maybe (Maybe, fromJust)
import Data.String (length)
import Data.String.CodeUnits (toCharArray, fromCharArray)
import Partial.Unsafe (unsafePartial)

-- That's right the largest levenshtein distance this can calculate is 32766
largest :: Int
largest = 32767

minimum :: Array Int -> Int
minimum arr = foldl (\x y -> min x y) largest $ arr

fromJust' :: forall a. Maybe a -> a
fromJust' = unsafePartial fromJust

stringInit :: String -> String
stringInit word = fromCharArray $ fromJust' $ init $ toCharArray word

stringLast :: String -> Char
stringLast word = fromJust' $ last $ toCharArray word

levenshtein :: String -> String -> Int
levenshtein "" "" = 0
levenshtein "" secondWord = length secondWord
levenshtein firstWord "" = length firstWord
levenshtein firstWord secondWord 
      | stringLast firstWord == stringLast secondWord = levenshtein (stringInit firstWord) (stringInit secondWord)
      | otherwise = minimum [1 + levenshtein (stringInit firstWord) secondWord,
                          1 + levenshtein firstWord (stringInit secondWord),
                          1 + levenshtein (stringInit firstWord) (stringInit secondWord)]