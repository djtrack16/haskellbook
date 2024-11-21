{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use <$>" #-}
{-# HLINT ignore "Eta reduce" #-}
module ChapterExercise where
  
  import Data.Monoid qualified as M
  import Data.Semigroup qualified as S
  import Test.QuickCheck
  import Test.QuickCheck.Checkers
  import Test.QuickCheck.Classes
  import Control.Applicative

  type Sum = S.Sum
  
{--
-- Given a type that has an instance of Applicative, specialize the types of the methods.
-- Test your specialization in the REPL.
1. -- Type
[]
-- Methods
pure ::a->?a
(<*>) :: ? (a -> b) -> ? a -> ? b
2. -- Type IO
-- Methods
pure ::a->?a
(<*>) :: ? (a -> b) -> ? a -> ? b
3. -- Type (,) a
-- Methods
pure ::a->?a
(<*>) :: ? (a -> b) -> ? a -> ? b
4. -- Type (->) e
-- Methods
pure ::a->?a
(<*>) :: ? (a -> b) -> ? a -> ? b
--}

-- #1
-- Type []
-- Methods
  listPure :: a -> [] a
  listPure = undefined

  listApply :: [] (a -> b) -> [] a -> [] b
  listApply = undefined

-- #2
-- Type IO
-- Methods
  iOPure :: a -> IO a
  iOPure = undefined

  iOApply :: IO (a -> b) -> IO a -> IO b
  iOApply = undefined

-- #3
-- Type (,) a
-- Methods
  tuplePure :: a -> (,) a a
  tuplePure = undefined

  tupleApply :: (,) a (a -> b) -> (,) a a -> (,) a b
  tupleApply = undefined

-- #4
-- Type (->) e
-- Methods
  funcPure :: a -> (->) e a
  funcPure = undefined

  funcApply :: (->) e (a -> b) -> (->) e a -> (->) e b
  funcApply = undefined

{--
Write applicative instances for the following datatypes. Confused?
Write out what the type should be. Use the checkers library to validate the instances.

1. newtype Identity a = Identity a deriving Show
2. data Pair a = Pair a a deriving Show
3. This should look familiar.
data Two a b = Two a b
4. data Three a b c = Three a b c
5. data Three' a b = Three' a b b
6. data Four a b c d = Four a b c d
7. data Four' a b = Four' a a a b

--}

-- 1.

  newtype Identity a =
    Identity a deriving (Eq, Show)

  instance Functor Identity where

    fmap :: (a -> b) -> Identity a -> Identity b
    fmap f (Identity a) = Identity (f a)
  
  instance Applicative Identity where

    pure :: a -> Identity a
    pure = Identity

    (<*>) :: Identity (a -> b) -> Identity a -> Identity b
    (<*>) (Identity f) (Identity a') = Identity (f a')
  
  instance Arbitrary a => Arbitrary (Identity a) where
    arbitrary = do
      a <- arbitrary
      return (Identity a)

  instance Eq a => EqProp (Identity a) where
    (=-=) = eq

  --testExample = undefined :: (String, String, Int)

-- 2. data Pair a = Pair a a deriving Show

  data Pair a =
    Pair a a deriving (Eq, Show)

  instance Functor Pair where

    fmap :: (a -> b) -> Pair a -> Pair b
    fmap f (Pair x y) = Pair (f x) (f y)
  
  instance Applicative Pair where

    pure :: a -> Pair a
    pure a = Pair a a

    (<*>) :: Pair (a -> b) -> Pair a -> Pair b
    (<*>) (Pair f a) (Pair a' b') = Pair (f a') (f b')

  
  instance (Arbitrary a) => Arbitrary (Pair a) where
    arbitrary = do
      a <- arbitrary
      --b <- arbitrary
      return (Pair a a)

  instance Eq a => EqProp (Pair a) where
    (=-=) = eq

-- #3
  data Two a b =
    Two a b
    deriving (Eq, Show)

  instance Functor (Two a) where

    fmap :: (a2 -> b) -> Two a1 a2 -> Two a1 b
    fmap f (Two a b) = Two a (f b)
  
  instance Monoid a => Applicative (Two a) where

    liftA2 :: Monoid a => (a1 -> b -> c) -> Two a a1 -> Two a b -> Two a c
    liftA2 f (Two a b) (Two a' b') = Two a (f b b')

    pure :: Monoid a => a1 -> Two a a1
    pure a = Two mempty a
  
  instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
    arbitrary = do
      a <- arbitrary
      b <- arbitrary
      return (Two a b)

  instance (Eq a, Eq b) => EqProp (Two a b) where
    (=-=) = eq

  testTypeOne = undefined :: (String, String, String)
  testTypeTwo = undefined :: (String, String, String)

  main :: IO ()
  main = do
    putStrLn "IDENTITY TESTS"
    quickBatch (applicative (Identity testTypeOne))
    putStrLn "PAIR TESTS"
    quickBatch (applicative (Pair testTypeOne testTypeOne))
    putStrLn "TWO TESTS"
    quickBatch (applicative (Two testTypeOne testTypeTwo))