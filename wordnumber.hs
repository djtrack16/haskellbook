{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Fuse foldr/map" #-}
module WordNumber where

  import Data.List (intercalate)
  
  digitToWord :: Int -> String
  digitToWord n = case n of
    0 -> "zero"
    1 -> "one"
    2 -> "two"
    3 -> "three"
    4 -> "four"
    5 -> "five"
    6 -> "six"
    7 -> "seven"
    8 -> "eight"
    9 -> "nine"
    _ -> error "invalid"

  digits :: Int -> [Int]
  digits n = if n < 10 then [n] else digits (div n 10) ++ [mod n 10]

  wordNumber :: Int -> String
  wordNumber n = intercalate "-" $ map digitToWord $ digits n