--{-# LANGUAGE UndecidableInstances #-}
module ChapterExercises where

  import Control.Monad
  import Data.Functor
  import Data.Monoid
  import Control.Applicative
  import Test.QuickCheck.Checkers
  import Test.QuickCheck
  import Test.QuickCheck.Classes
  --import ChapterSeventeen.List qualified as L
  
-- 1. Welcome to the Nope Monad, where nothing happens and no- body cares.

  data Nope a = NopeDotJpg deriving (Eq, Show)


  instance Functor Nope where
    fmap :: (a -> b) -> Nope a -> Nope b
    fmap _ _ = NopeDotJpg


  instance Applicative Nope where
    pure :: a -> Nope a
    pure = undefined

    (<*>) :: Nope (a -> b) -> Nope a -> Nope b
    (<*>) _ _ = NopeDotJpg


  instance Monad Nope where
    return = pure

    (>>=) :: Nope a -> (a -> Nope b) -> Nope b
    (>>=) _ _ = NopeDotJpg

  --instance Arbitrary a => (Nope a) where
  --  fmap NopeDotJpg arbitrary

  instance (Eq a) => EqProp (Nope a) where
    (=-=) = eq

-- 2. data PhhhbbtttEither b a = Left a | Right b

  data PhhhbbtttEither b a =
    Left' a
    | Right' b
    deriving (Eq, Show)

  instance Functor (PhhhbbtttEither b) where
    fmap :: (a -> b2) -> PhhhbbtttEither b1 a -> PhhhbbtttEither b1 b2
    fmap f (Left' a) = Left' (f a)
    fmap _ (Right' b) = Right' b

  instance Applicative (PhhhbbtttEither b) where
    pure :: a -> PhhhbbtttEither b a
    pure = Left'

    (<*>) :: PhhhbbtttEither b1 (a -> b2) -> PhhhbbtttEither b1 a -> PhhhbbtttEither b1 b2
    (<*>) (Left' f) (Left' a) = Left' (f a)
    (<*>) (Left' f) (Right' b) = Right' b
    (<*>) (Right' b) (Left' a) = Right' b
    (<*>) (Right' b) (Right' b') = Right' b

  instance Monad (PhhhbbtttEither b) where
    return = pure

    (>>=) :: PhhhbbtttEither b1 a -> (a -> PhhhbbtttEither b1 b2) -> PhhhbbtttEither b1 b2
    (>>=) (Left' a) f = f a
    (>>=) (Right' b) f = Right' b


  instance (Eq a, Eq b) => EqProp (PhhhbbtttEither b a) where
    (=-=) = eq

  instance (Arbitrary a, Arbitrary b) => Arbitrary (PhhhbbtttEither a b) where
    arbitrary = do
      a <- arbitrary
      b <- arbitrary
      frequency [ (1, return (Left' a)) , (1, return (Right' b)) ]

-- 3. Write a Monad instance for Identity.

  newtype Identity a =
    Identity a
    deriving (Eq, Ord, Show)

  instance Functor Identity where
    fmap :: (a -> b) -> Identity a -> Identity b
    fmap f (Identity a) = Identity $ f a

  instance Applicative Identity where

    pure :: a -> Identity a
    pure = Identity

    (<*>) :: Identity (a -> b) -> Identity a -> Identity b
    (<*>) (Identity f) (Identity a) = Identity $ f a

  instance Monad Identity where
    return = pure

    (>>=) :: Identity a -> (a -> Identity b) -> Identity b
    (>>=) (Identity a) f = f a

  instance Eq a => EqProp (Identity a) where
    (=-=) = eq

  instance Arbitrary a => Arbitrary (Identity a) where
    arbitrary = do
      Identity <$> arbitrary

-- 4. This one should be easier than the Applicative instance was. Remember to use the Functor that Monad requires, then see where the chips fall.

  data List a =
    Nil
    | Cons a (List a)
    deriving (Eq, Show)

  append :: List a -> List a -> List a
  append Nil ys = ys
  append (Cons x xs) ys = Cons x $ xs `append` ys

  instance Functor List where

    fmap :: (a -> b) -> List a -> List b
    fmap f Nil = Nil
    fmap f (Cons a list) = Cons (f a) (fmap f list)

  instance Applicative List where
    pure :: a -> List a
    pure a = Cons a Nil

    (<*>) :: List (a -> b) -> List a -> List b
    (<*>) (Cons f listF) listA = append (fmap f listA) (listF <*> listA)
    (<*>) _ Nil = Nil
    (<*>) Nil _ = Nil


  instance Monad List where

    (>>=) :: List a -> (a -> List b) -> List b
    (>>=) Nil f = Nil
    (>>=) (Cons a list) f = append (f a) $ (>>=) list f

  exampleList :: List (String, String, String)
  exampleList = Cons testType (Cons testType Nil)

  instance (Arbitrary a) => Arbitrary (List a) where
    arbitrary :: Arbitrary a => Gen (List a)
    arbitrary = oneof [
        return Nil,
        Cons <$> arbitrary <*> arbitrary
      ]

  instance Eq a => EqProp (List a) where
    (=-=) = eq

  testType = undefined :: (String, String, String)

  main :: IO()
  main = do
    --let var = ZipList'
    --let var = (Char, Char, Char)
    --quickBatch (monad (undefined :: Nope (Char, Char, Char)))
    quickBatch (monad (undefined :: PhhhbbtttEither (Char, Char, Char) (Char, Char, Char)))
    quickBatch (monad (undefined :: Identity (Char, Char, Char)))
    quickBatch (monad exampleList)