--{-# LANGUAGE UndecidableInstances #-}
--{-# LANGUAGE AllowAmbiguousTypes #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use <$>" #-}
{-# HLINT ignore "Monoid law, left identity" #-}
{-# HLINT ignore "Monoid law, right identity" #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# HLINT ignore "Redundant bracket" #-}
{-# LANGUAGE MonoLocalBinds #-}

module SemigroupExercises where

  -- starts at page 586
{--
  Write Semigroup instances for the following types
  1. data Trivial = Trivial deriving (Eq, Show)
  2. newtype Identity a = Identity a
  3. data Two a b = Two a b
  4. data Three a b c = Three a b c
  5. newtype BoolConj = BoolConj Bool
  6. newtype data Or a b = Fst a | Snd b
  9. newtype Combine a b = Combine { unCombine :: (a -> b) }
--}
  import Control.Monad
  import qualified Data.Monoid as M
  import qualified Data.Semigroup as S
  import Test.QuickCheck hiding (Success, Failure)
  --import Test.QuickCheck.Poly (OrdA(OrdA))
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

  -- BoolConj
  newtype BoolConj = BoolConj Bool
    deriving (Eq, Show)

  instance Monoid BoolConj where
    mempty :: BoolConj
    mempty = BoolConj True

  instance S.Semigroup BoolConj where
    (<>) :: BoolConj -> BoolConj -> BoolConj
    (<>) (BoolConj True) (BoolConj True) = BoolConj True
    (<>) (BoolConj False) _ = BoolConj False
    (<>) _ (BoolConj False) = BoolConj False

  instance Arbitrary BoolConj where
    arbitrary :: Arbitrary Bool => Gen BoolConj
    arbitrary = do
      a <- elements [True, False]
      return (BoolConj a)

  type BoolConjAssoc a = BoolConj -> BoolConj -> BoolConj -> Bool

  -- SIX

  data Or a b = Fst a
    | Snd b deriving (Eq, Show)

  instance (Monoid a, Monoid b) => Monoid (Or a b) where
    mempty :: (Monoid a, Monoid b) => Or a b
    mempty = Fst mempty

  instance (Semigroup a, Semigroup b) => S.Semigroup (Or a b) where
    (<>) :: Or a b -> Or a b -> Or a b
    (<>) (Snd x) _ = Snd x
    (<>) _ (Snd x) = Snd x
    (<>) x y = y


  instance (Arbitrary a, Arbitrary b) => Arbitrary (Or a b) where
    --arbitrary :: (Arbitrary a, Arbitrary b) => Gen (a, b)
    arbitrary = do
      z <- arbitrary
      y <- arbitrary
      elements [Fst y, Snd z]

  type OrAssoc a b = Or a b -> Or a b -> Or a b -> Bool

  -- VALIDATION

  data Validation a b =
    Failure a
    | Success b
    deriving (Eq, Show)

  instance (Monoid a, Monoid b) => Monoid (Validation a b) where
    mempty :: (Monoid a, Monoid b) => Validation a b
    mempty = Success mempty

  instance (Semigroup a, Semigroup b) => Semigroup (Validation a b) where

    (<>) :: (Semigroup a, Semigroup b) => Validation a b -> Validation a b -> Validation a b
    (<>) (Failure x) (Failure y) = Failure (x S.<> y)
    (<>) y (Failure x) = (Failure x)
    (<>) (Failure x) _ = Failure x
    (<>) (Success x) (Success y) = Success (x S.<> y)
  
  instance (Arbitrary a, Arbitrary b) => Arbitrary (Validation a b) where
    arbitrary = do
      z <- arbitrary
      y <- arbitrary
      elements [Failure y, Success z]
  
  type ValidationAssoc a b = Validation a b -> Validation a b -> Validation a b -> Bool

  --newtype Combine a b = Combine { unCombine :: (a -> b) }

  newtype Combine a b = Combine
    { unCombine :: (a -> b) }
    --deriving (Eq, Show)

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

  testBoolConj :: IO()
  testBoolConj = do
    quickCheck (semigroupAssoc :: BoolConjAssoc Bool)
    quickCheck (monoidLeftIdentity :: BoolConj -> Bool)
    quickCheck (monoidRightIdentity :: BoolConj -> Bool)

  testOr :: IO()
  testOr = do
    --quickCheck (semigroupAssoc :: OrAssoc String [String])
    --quickCheck (monoidLeftIdentity :: Or [String] String -> Bool)
    --quickCheck (monoidRightIdentity :: Or String [String] -> Bool)
   undefined

  testValidation :: IO()
  testValidation = do
    quickCheck (semigroupAssoc :: ValidationAssoc String [String])
    quickCheck (monoidLeftIdentity :: Validation [String] String -> Bool)
    quickCheck (monoidRightIdentity :: Validation String [String] -> Bool)


  main :: IO ()
  main = do
    testTrivial
    testIdentity
    testTwo
    testThree
    testBoolConj
    --testOr
    testValidation