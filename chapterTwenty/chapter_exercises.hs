{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}
{-# HLINT ignore "Eta reduce" #-}
{-# HLINT ignore "Use <$>" #-}
{-# HLINT ignore "Redundant ==" #-}
module ChapterExercises where


  import Data.Foldable
  import Data.Monoid
  import Test.Hspec
  import Test.QuickCheck
  import Test.QuickCheck.Checkers

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

  main :: IO()
  main = hspec runTests

  runTests :: SpecWith()
  runTests = do
    describe "Built-in functions" $ do
      it "filterF should work" $ do
        filterF odd [1..5] `shouldBe` [1,3,5]
        filterF (True ==) [True, False, True, False] `shouldBe` [True, True]
        filterF (elem 1) [[1,2],[3,4],[5,6]] `shouldBe` [[1,2]]
        filterF null [Left 5] `shouldBe` ([Left 5] :: [Either Int Int])
        filterF null [Right 5] `shouldBe` ([] :: [Either Int Int])
        filterF null [Nothing] `shouldBe` ([Nothing] :: [Maybe Int])
        filterF null [Just 1] `shouldBe` ([] :: [Maybe Int])
        