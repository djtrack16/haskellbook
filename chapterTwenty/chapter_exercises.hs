{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}
{-# HLINT ignore "Eta reduce" #-}
{-# HLINT ignore "Use <$>" #-}
module ChapterExercises where


  import Data.Foldable
  import Data.Monoid
  import Data.Functor
  import Test.Hspec
  --import Test.Hspec.Checkers
  import Test.QuickCheck
  import Test.QuickCheck.Checkers
  import Test.QuickCheck.Classes hiding (bind)
  
{--
Write Foldable instances for the following datatypes.
1. data Constant a b =
Constant a
2. data Two a b =
Two a b
3. data Three a b c =
Three a b c
4. data Three' a b =
Three' a b b
5. data Four' a b =
Four' a b b b
--}

-- 1.

  data Constant a b =
    Constant a
    deriving (Eq, Show)

  instance Foldable (Constant a) where

    foldr :: (a2 -> b -> b) -> b -> Constant a1 a2 -> b
    foldr f b' (Constant a) = b'

-- 2.

  data Two a b =
    Two a b

  instance Foldable (Two a) where

    foldr :: (a2 -> b -> b) -> b -> Two a1 a2 -> b
    foldr f b' (Two a b) = f b b'

-- 3.

  data Three a b c =
    Three a b c

  instance Foldable (Three a b) where

    foldr :: (a2 -> b2 -> b2) -> b2 -> Three a1 b1 a2 -> b2
    foldr f b (Three a b' c) = f c b


-- 4.

  data Three' a b =
    Three' a b b

  instance Foldable (Three' a) where

    foldMap :: Monoid m => (a2 -> m) -> Three' a1 a2 -> m
    foldMap f (Three' a b b') = f b

-- 5.

  data Four' a b =
    Four' a b b b

  instance Foldable (Four' a) where

    foldMap :: Monoid m => (a2 -> m) -> Four' a1 a2 -> m
    foldMap f (Four' a b1 b2 b3) = f b1

    foldr :: (a2 -> b -> b) -> b -> Four' a1 a2 -> b
    foldr f b' (Four' a b1 b2 b3) = f b1 b'

-- Thinking cap time. Write a filter function for Foldable types using foldMap.

  filterF :: (Applicative f, Foldable f, Monoid (f a)) => (a -> Bool) -> f a -> f a
  filterF predicate = foldMap (\a -> if predicate a then pure a else mempty)

  testType = undefined :: (String, String, String)

  instance Arbitrary a => Arbitrary (Constant a b) where
    arbitrary = do
      a <- arbitrary
      return (Constant a)

  instance (Eq a, Eq b) => EqProp (Constant a b) where
    (=-=) = eq

  main :: IO()
  main = do
    putStrLn  ""
   -- quickBatch (foldable (
   --   undefined :: Two (String, String, String, String, String) (String, String, String, Int, Int))
   --   )