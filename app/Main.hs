module Main where

import Lib (levenshtein)
import System.Environment (getArgs)

main :: IO ()
main = do
  args <- getArgs
  print $ levenshtein (head args) (args !! 1)
