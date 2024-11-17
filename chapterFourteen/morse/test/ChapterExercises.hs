{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use concat" #-}
module ChapterExercises where

  --import Test.QuickCheck

  halfingInts :: Eq a => Fractional a => a -> Bool
  halfingInts n = n == halfIdentity where
    half = n / 2
    halfIdentity = 2 * half

  plusAssociative :: Int -> Int -> Int -> Bool
  plusAssociative x y z = x + (y + z) == (x + y) + z

  plusCommutative :: Int -> Int -> Bool
  plusCommutative x y = x + y == y + x

  multAssociative :: Int -> Int -> Int -> Bool
  multAssociative x y z = x * (y * z) == (x * y) * z

  multCommutative :: Double -> Double -> Bool
  multCommutative x y = x * y == y * x


  quotRemIdentity :: Int -> Int -> Bool
  quotRemIdentity _ 0 = True
  quotRemIdentity x y = quot x y*y + rem x y == x

  divModIdentity :: Int -> Int -> Bool
  divModIdentity _ 0 = True
  divModIdentity x y = div x y*y + mod x y == x

  concatEquality :: [[Double]] -> Bool
  concatEquality xs = foldr (++) [] xs == concat xs

  consAndConcatEquality :: [Double] -> Bool
  consAndConcatEquality xs = foldr (:) [] xs == (++) [] xs