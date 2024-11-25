{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use sum" #-}
{-# HLINT ignore "Eta reduce" #-}
{-# HLINT ignore "Use product" #-}
{-# HLINT ignore "Redundant bracket" #-}
{-# HLINT ignore "Evaluate" #-}
{-# HLINT ignore "Use max" #-}
module ShortExercises where
  import Data.Foldable
  import Data.Monoid
  import Test.Hspec
  import Test.QuickCheck
  import Test.QuickCheck.Checkers

-- Implement the functions in terms of foldMap or foldr from Foldable,
-- then try them out with multiple types that have Foldable instances.

  -- 1. This and the next one are nicer with foldMap, but foldr is fine too.
  sum' :: (Foldable t, Num a) => t a -> a
  sum' as = foldr (+) 0 as
  
  -- #2
  product' :: (Foldable t, Num a) => t a -> a
  product' = foldr (*) 1

  -- #3
  elem' :: (Foldable t, Eq a) => a -> t a -> Bool
  elem' x = foldr (\a b -> b || (x == a)) False

  -- #4.
  minimum' :: (Foldable t, Ord a) => t a -> Maybe a
  minimum' = foldr f Nothing where
    f x init = case init of
      Nothing -> Just x
      _ -> min (Just x) init

  -- #5.
  maximum' :: (Foldable t, Ord a) => t a -> Maybe a
  maximum' = foldr f Nothing where
    f x init = case init of
      Nothing -> Just x
      _ -> max (Just x) init

  -- #6.
  null' :: (Foldable t) => t a -> Bool
  null' = foldr f True where f _ _ = False

  -- #7.
  length' :: (Foldable t) => t a -> Int
  length' = foldr (\_ init -> init + 1) 0

  -- #8. Some say this is all Foldable amounts to.
  toList' :: (Foldable t) => t a -> [a]
  toList' = foldMap (:[]) 

  -- #9. Hint: use foldMap.
  -- Combine the elements of a structure using a monoid.
  fold' :: (Foldable t, Monoid m) => t m -> m
  fold' = foldMap (mappend mempty)

  -- #10. Define foldMap in terms of foldr.
  myFoldMap' :: (Foldable t, Monoid m) => (a -> m) -> t a -> m
  myFoldMap' f = foldr (\x init -> (f x) <> init) mempty

  main :: IO ()
  main = hspec runTests

  runTests :: SpecWith()
  runTests = do
    describe "Built-in functions" $ do
      it "sum should work" $ do
        sum' [1, 2, 3, 4] `shouldBe` sum [1, 2, 3, 4]
        sum' [-1, -2, -3, -4] `shouldBe` sum [-1, -2, -3, -4]
        sum' Nothing `shouldBe` sum Nothing
        sum' (Just 10) `shouldBe` sum (Just 10)
        sum' (Sum 10) `shouldBe` sum (Just 10)
        sum' [Sum 10, Sum 20, Sum 30] `shouldBe` Sum 60
        sum' (Right 5) `shouldBe` sum (Right 5)
      it "product should work" $ do
        product' [1, 2, 3, 4] `shouldBe` product [1, 2, 3, 4]
        product' [1, -2, -3, -4] `shouldBe` product [1, -2, -3, -4]
        product' Nothing `shouldBe` product Nothing
        product' (Sum 10) `shouldBe` product (Just 10)
        product' [Product 10, Product 20, Product 30] `shouldBe` Product 6000
        product' (Left 5) `shouldBe` product (Left 5)
      it "elem should work" $ do
        elem' 2 [1, 2, 3, 4] `shouldBe` elem 2 [1, 2, 3, 4]
        elem' 0 [1, -2, -3, -4] `shouldBe` elem 0 [1, -2, -3, -4]
        elem' (Just 1) Nothing `shouldBe` elem (Just 1) Nothing
        elem' 10 (Sum 10) `shouldBe` elem 10 (Sum 10)
        elem' (Product 20) [Product 10, Product 20] `shouldBe` elem (Product 20) [Product 10, Product 20]
        elem' 5 (Left 5) `shouldBe` False
        elem' 5 (Right 5) `shouldBe` True
      it "minimum should work" $ do
        minimum' [0, 2, 3, 4] `shouldBe` Just 0
        minimum' [] `shouldBe` (Nothing :: Maybe Char)
        minimum' Nothing `shouldBe` (Nothing :: Maybe Int)
      it "maximum should work" $ do
        maximum' [1, 2, 3, 4] `shouldBe` Just 4
        maximum' [] `shouldBe` (Nothing :: Maybe Int)
        maximum' ["ab", "c"] `shouldBe` maximum [Just "ab", Just "c"]
        maximum' ['a', 'b'] `shouldBe` maximum [Just 'a', Just 'b', Nothing]
      it "null should work" $ do
        null' [1, 2, 3, 4] `shouldBe` null [1, 2, 3, 4]
        null' [] `shouldBe` null []
        null' (Just 2) `shouldBe` null (Just 2)
        null' (Left 4) `shouldBe` null (Left 4)
        null' (Right True) `shouldBe` null (Right False)
      it "length should work" $ do
        length' [1, 2, 3, 4] `shouldBe` length [1, 2, 3, 4]
        length' [] `shouldBe` length []
        length' (Just 2) `shouldBe` length (Just 2)
        length' Nothing `shouldBe` length Nothing
        fmap length' [Just 1, Just 2, Just 3] `shouldBe` fmap length [Just 1, Just 2, Just 3]
        fmap length' [Just 1, Just 2, Nothing] `shouldBe` fmap length [Just 1, Just 2, Nothing] 
      it "toList should work" $ do
        toList' [1, 2, 3, 4] `shouldBe` toList [1, 2, 3, 4]
        toList' [] `shouldBe` toList ([] :: [Int])
        toList' (Just 10) `shouldBe` toList (Just 10)
        toList' Nothing `shouldBe` toList (Nothing :: Maybe Int)
      it "fold should work" $ do
        fold' ["12", "34", "56"] `shouldBe` fold ["12", "34", "56"]
        fold' [] `shouldBe` fold ([] :: [String])
        fold' (Just "1") `shouldBe` fold (Just "1")
      it "foldMap should work" $ do
        myFoldMap' Sum [1, 2, 3, 4] `shouldBe` foldMap Sum [1, 2, 3, 4]
        myFoldMap' Product [1,2,3] `shouldBe` foldMap Product [1,2,3]
        myFoldMap' All (Just True) `shouldBe` foldMap All (Just True)
        myFoldMap' First [Just 1] `shouldBe` foldMap First [Just 1]

          
