{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use <$>" #-}
module Validation where

  import Test.QuickCheck hiding (Success)
  import Test.QuickCheck.Checkers
  import Test.QuickCheck.Classes

  data Sum a b =
    First a
    | Second b
    deriving (Eq, Show)

  data Validation e a =
    Error e
    | Success a
    deriving (Eq, Show)

  instance Functor (Sum a) where

    fmap :: (a2 -> b) -> Sum a1 a2 -> Sum a1 b
    fmap _ (First a)  = First a
    fmap f (Second b) = Second (f b)

  instance Applicative (Sum a) where

    pure :: a2 -> Sum a1 a2
    pure = Second

    (<*>) :: Sum a1 (a2 -> b) -> Sum a1 a2 -> Sum a1 b
    (<*>) (First a) (First a') = First a
    (<*>) (First a) (Second b) = First a
    (<*>) (Second f) (First a) = First a
    (<*>) (Second f) (Second b) = Second (f b)


  -- same as Sum/Either
  instance Functor (Validation e) where

    fmap :: (a -> b) -> Validation e a -> Validation e b
    fmap _ (Error e) = Error e
    fmap f (Success a) = Success (f a)

  -- This is different
  instance Monoid e => Applicative (Validation e) where

    pure :: Monoid e => a -> Validation e a
    pure = Success

    (<*>) :: Monoid e => (Validation e) (a -> b) -> (Validation e) a -> (Validation e) b
    (<*>) (Error e) (Error e') = Error (e <> e')
    (<*>) (Error e) (Success a) = Error e
    (<*>) (Success f) (Error e) = Error e
    (<*>) (Success f) (Success a) = Success (f a)


 -- Your hint for this one is that you’re writing the following functions:
  --applyIfBothSecond :: (Sum e) (a -> b) -> (Sum e) a -> (Sum e) b
  --applyIfBothSecond = undefined

  --applyMappendError :: Monoid e => (Validation e) (a -> b) -> (Validation e) a -> (Validation e) b
  --applyMappendError = undefined

  instance (Arbitrary a, Arbitrary b) => Arbitrary (Sum a b) where
    arbitrary = do
      a <- arbitrary
      b <- arbitrary
      frequency [ (1, return (First a)) , (1, return (Second b)) ]

  instance (Eq a, Eq b) => EqProp (Sum a b) where
    (=-=) = eq

  instance (Arbitrary a, Arbitrary e) => Arbitrary (Validation e a) where
    arbitrary = do
      a <- arbitrary
      e <- arbitrary
      frequency [ (1, return (Error e)) , (1, return (Success a)) ]

--  arbitrary = Sum <$> arbitrary

  instance (Eq a, Eq e) => EqProp (Validation e a) where
    (=-=) = eq

  main :: IO()
  main = do
    quickBatch (applicative (undefined :: Sum String (Char, Char, Char)))
    quickBatch (applicative (undefined :: Validation String (String, Int, Int)))