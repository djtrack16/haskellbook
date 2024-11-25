{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use =<<" #-}
module ShortExercises where

  import Control.Monad
  import Data.Functor
  import Control.Applicative
  import Test.QuickCheck.Checkers --(quickBatch, EqProp)
  import Test.QuickCheck
  import Test.QuickCheck.Classes
  --import Data.Semigroup qualified as S

  bind :: Monad m => (a -> m b) -> m a -> m b
  bind f as = join $ fmap f as

  --join' :: Monad m => m (m a) -> m a
  --join' = undefined

  --fmap' :: Functor f => (a -> f b) -> f a -> f b
  --fmap' = undefined

-- Implement the Either Monad

  data Sum a b =
    First a
    | Second b
    deriving (Eq, Show)
  
  instance Functor (Sum a) where
    fmap :: (a2 -> b) -> Sum a1 a2 -> Sum a1 b
    fmap f (First a) = First a
    fmap f (Second b) = Second (f b)

  instance Applicative (Sum a) where

    pure :: a2 -> Sum a1 a2
    pure = Second

    (<*>) :: Sum a1 (a2 -> b) -> Sum a1 a2 -> Sum a1 b
    (<*>) (First a) (First a') = First a
    (<*>) (First a) (Second b) = First a
    (<*>) (Second f) (First a) = First a
    (<*>) (Second f) (Second b) = Second (f b)

  
  instance Monad (Sum a) where
    
    return :: a2 -> Sum a1 a2
    return = pure
    
    (>>=) :: Sum a1 a2 -> (a2 -> Sum a1 b) -> Sum a1 b
    (>>=) (First a) _ = First a
    (>>=) (Second b) f = f b

  instance (Eq a, Eq e) => EqProp (Sum e a) where
    (=-=) = eq

  instance (Arbitrary a, Arbitrary b) => Arbitrary (Sum a b) where
    arbitrary = do
      a <- arbitrary
      b <- arbitrary
      frequency [ (1, return (First a)) , (1, return (Second b)) ]

  main :: IO()
  main = do
    quickBatch (monad (undefined :: Sum String (Char, Char, Char)))
    quickBatch (monad (undefined :: Sum (Char, Char, Char) (Char, Char, Char)))
