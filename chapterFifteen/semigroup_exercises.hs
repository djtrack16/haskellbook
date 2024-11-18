--{-# LANGUAGE UndecidableInstances #-}
--{-# LANGUAGE AllowAmbiguousTypes #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use <$>" #-}
{-# HLINT ignore "Monoid law, left identity" #-}
{-# HLINT ignore "Monoid law, right identity" #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# HLINT ignore "Redundant bracket" #-}

module SemigroupExercises where

  -- starts at page 586
{--
  EXERCISES:
  1. data Trivial = Trivial deriving (Eq, Show)
  2. newtype Identity a = Identity a
  3. data Two a b = Two a b
  4. data Three a b c = Three a b c
--}
  import Control.Monad
  import qualified Data.Monoid as M
  import qualified Data.Semigroup as S
  import Test.QuickCheck
  --import ChapterFifteen

  -- TRIVIAL

  data Trivial = Trivial deriving (Eq, Show)

  --instance Monoid a => Monoid Trivial where
    --mempty :: Monoid a => Trivial
  --  mempty = Trivial

  instance S.Semigroup Trivial where
    _ <> _ = Trivial

  instance Arbitrary Trivial where
    arbitrary = return Trivial

  type TrivialAssoc = Trivial -> Trivial -> Trivial -> Bool

  -- IDENTITY
  newtype Identity a =
    Identity a deriving (Eq, Show)

  instance Monoid a => Monoid (Identity a) where
    mempty :: Monoid a => Identity a
    mempty = Identity mempty

  instance (Semigroup a) => S.Semigroup (Identity a) where
    (<>) :: Identity a -> Identity a -> Identity a
    (<>) (Identity x) (Identity y) = Identity (x S.<> y)

  instance Arbitrary a => Arbitrary (Identity a) where
    arbitrary :: Arbitrary a => Gen (Identity a)
    arbitrary = do
      a <- arbitrary
      return (Identity a)

  type IdentityAssoc t = Identity t -> Identity t -> Identity t -> Bool

  -- TWO

  data Two a b = Two a b deriving (Eq, Show)

  instance (Monoid a, Monoid b) => Monoid (Two a b) where
    mempty :: (Monoid a, Monoid b) => Two a b
    mempty = Two mempty mempty

  instance (Semigroup a, Semigroup b) => S.Semigroup (Two a b) where
    (<>) :: Two a b -> Two a b -> Two a b
    (<>) (Two w x) (Two y z) = Two first last where
      first = w S.<> y
      last = x S.<> z

  instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
    --arbitrary :: (Arbitrary a, Arbitrary b) => Gen (a, b)
    arbitrary = do
      a <- arbitrary
      b <- arbitrary
      return (Two a b)

  type TwoAssoc a b = Two a b -> Two a b -> Two a b -> Bool

  -- THREE

  data Three a b c = Three a b c deriving (Eq, Show)

  instance (Monoid a, Monoid b, Monoid c) => Monoid (Three a b c) where
    mempty :: (Monoid a, Monoid b, Monoid c) => Three a b c
    mempty = Three mempty mempty mempty

  instance (Semigroup a, Semigroup b, Semigroup c) => S.Semigroup (Three a b c) where
    (<>) :: Three a b c -> Three a b c -> Three a b c 
    (<>) (Three a1 a2 a3) (Three b1 b2 b3) = Three first middle last where
      first = a1 S.<> b1
      middle = a2 S.<> b2
      last = a3 S.<> b3

  instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Three a b c) where
    arbitrary = do
      a <- arbitrary
      b <- arbitrary
      c <- arbitrary
      return (Three a b c)

  type ThreeAssoc a b c = Three a b c -> Three a b c -> Three a b c -> Bool


  -- associativity, left identity, right identity

  semigroupAssoc :: (Eq m, S.Semigroup m) => m -> m -> m -> Bool
  semigroupAssoc a b c = (a S.<> (b S.<> c)) == ((a S.<> b) S.<> c)

  monoidLeftIdentity :: (Eq m, M.Monoid m) => m -> Bool
  monoidLeftIdentity a = (mempty <> a) == a

  monoidRightIdentity :: (Eq m, M.Monoid m) => m -> Bool
  monoidRightIdentity a = (a <> mempty) == a

-- (Arbitrary a, Arbitrary b) => Arbitrary (a, b)

  testTrivial :: IO ()
  testTrivial = do
    quickCheck (semigroupAssoc :: TrivialAssoc)
    --quickCheck (monoidLeftIdentity :: Trivial -> Bool)
    --quickCheck (monoidRightIdentity :: Trivial -> Bool)

  testIdentity :: IO()
  testIdentity = do
    quickCheck (semigroupAssoc :: IdentityAssoc String)
    quickCheck (monoidLeftIdentity :: Identity [String] -> Bool)
    quickCheck (monoidRightIdentity :: Identity [String] -> Bool)

  testTwo :: IO()
  testTwo = do
    quickCheck (semigroupAssoc :: TwoAssoc String [String])
    quickCheck (monoidLeftIdentity :: Two String String -> Bool)
    quickCheck (monoidRightIdentity :: Two [String] String -> Bool)

  testThree :: IO()
  testThree = do
    quickCheck (semigroupAssoc :: ThreeAssoc String [String] String)
    quickCheck (monoidLeftIdentity :: Three String String String -> Bool)
    quickCheck (monoidRightIdentity :: Three [String] String [String] -> Bool)

  main :: IO ()
  main = do
    putStrLn "asd"
    testTrivial
    testIdentity
    testTwo
    testThree

