module Main where

  import qualified Data.Map as M
  import Morse
  import Test.QuickCheck
  import ChapterExercises
  
  allowedChars :: [Char]
  allowedChars = M.keys letterToMorse

  allowedMorse :: [Morse]
  allowedMorse = M.elems letterToMorse

  charGen :: Gen Char
  charGen = elements allowedChars

  morseGen :: Gen Morse
  morseGen = elements allowedMorse

  prop_thereAndBackAgain :: Property
  prop_thereAndBackAgain =
    forAll charGen
      (\c -> (charToMorse c >>= morseToChar) == Just c)

  main :: IO ()
  main = do
    quickCheck prop_thereAndBackAgain
    quickCheck plusAssociative
    quickCheck plusCommutative
    quickCheck multAssociative
    quickCheck multCommutative
    quickCheck divModIdentity
    quickCheck quotRemIdentity
    quickCheck consAndConcatEquality
    quickCheck concatEquality