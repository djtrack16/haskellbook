{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use <$>" #-}
module ChapterExercises where

  import Data.Traversable
  import Test.QuickCheck.Checkers
  import Test.QuickCheck
  import Test.QuickCheck.Classes
  import Control.Applicative (liftA3, liftA2)
  import Data.Monoid
  
  newtype Identity a =
    Identity a
    deriving (Eq, Ord, Show)

  instance Functor Identity where

    fmap :: (a -> b) -> Identity a -> Identity b
    fmap f (Identity a) = Identity (f a)


  instance Foldable Identity where

    foldr :: (a -> b -> b) -> b -> Identity a -> b
    foldr f b (Identity a) = f a b

  instance Traversable Identity where

    traverse :: Applicative f => (a -> f b) -> Identity a -> f (Identity b)
    traverse f (Identity a) = Identity <$> f a


  instance (Arbitrary a) => Arbitrary (Identity a) where
    arbitrary = do
      fmap Identity arbitrary

  instance Eq a => EqProp (Identity a) where
    (=-=) = eq

  newtype Constant a b =
    Constant { getConstant :: a }
    deriving (Eq, Show)

  instance Functor (Constant a) where
    fmap :: (a2 -> b) -> Constant a1 a2 -> Constant a1 b
    fmap _ (Constant a) = Constant a


  instance Foldable (Constant a) where
    foldr :: (a2 -> b -> b) -> b -> Constant a1 a2 -> b
    foldr _ b _ = b


  instance Traversable (Constant a) where
    sequenceA :: Applicative f => Constant a1 (f a2) -> f (Constant a1 a2)
    sequenceA (Constant a) = pure (Constant a)

  instance (Arbitrary a) => Arbitrary (Constant a b) where
    arbitrary = do
      fmap Constant arbitrary

  instance Eq a => EqProp (Constant a b) where
    (=-=) = eq

  data Optional a =
    Nada
    | Yep a
    deriving (Eq, Show)

  instance Functor Optional where

    fmap :: (a -> b) -> Optional a -> Optional b
    fmap f (Yep a) = Yep (f a)
    fmap f Nada    = Nada

  instance Foldable Optional where

    foldr :: (a -> b -> b) -> b -> Optional a -> b
    foldr f b (Yep a) = f a b
    foldr f b Nada    = b

  instance Traversable Optional where
    traverse :: Applicative f => (a -> f b) -> Optional a -> f (Optional b)
    traverse f (Yep a) = fmap Yep (f a)
    traverse f Nada = pure Nada

  instance (Arbitrary a) => Arbitrary (Optional a) where
    arbitrary = oneof [
        return Nada,
        Yep <$> arbitrary
      ]

  instance Eq a => EqProp (Optional a) where
    (=-=) = eq

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

  instance Foldable List where

    foldMap :: Monoid m => (a -> m) -> List a -> m
    foldMap f (Cons a list) = f a <> foldMap f list
    foldMap _ Nil           = mempty

    foldr :: (a -> b -> b) -> b -> List a -> b
    foldr f b (Cons a list) = foldr f (f a b) list
    foldr _ b Nil           = b


  instance Traversable List where

    traverse :: Applicative f => (a -> f b) -> List a -> f (List b)
    traverse f (Cons a list) = first <*> rest where
                               first = fmap Cons (f a)
                               rest  = traverse f list
    traverse _ Nil           = pure Nil

  instance (Arbitrary a) => Arbitrary (List a) where
    arbitrary :: Arbitrary a => Gen (List a)
    arbitrary = oneof [
        return Nil,
        liftA2 Cons arbitrary arbitrary
      ]

  instance Eq a => EqProp (List a) where
    xs =-= ys = take' 3000 xs `eq` take' 3000 ys

  take' :: Int -> List a -> List a
  take' n Nil = Nil
  take' n (Cons a list) = Cons a (take' (n-1) list)


  data Three a b c =
    Three a b c
    deriving (Eq, Show)

  instance Functor (Three a b) where
    fmap :: (a2 -> b2) -> Three a1 b1 a2 -> Three a1 b1 b2
    fmap f (Three a b c) = Three a b (f c)


  instance Foldable (Three a b) where
    foldr :: (a2 -> b2 -> b2) -> b2 -> Three a1 b1 a2 -> b2
    foldr f b (Three a b' c) = f c b


  instance Traversable (Three a b) where
    traverse :: Applicative f => (a2 -> f b2) -> Three a1 b1 a2 -> f (Three a1 b1 b2)
    traverse f (Three a b c) = fmap (Three a b) (f c)

  instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Three a b c) where
    --arbitrary :: (Arbitrary a, Arbitrary b) => Gen (Three a b c)
    arbitrary = do
      a <- arbitrary
      b <- arbitrary
      c <- arbitrary
      return (Three a b c)

  instance (Eq a, Eq b, Eq c) => EqProp (Three a b c) where
    (=-=) = eq

  
  data Three' a b =
    Three' a b b
    deriving (Eq, Show)

  instance Functor (Three' a) where

    fmap :: (a2 -> b) -> Three' a1 a2 -> Three' a1 b
    fmap f (Three' a b b') = Three' a (f b) (f b')

  instance Foldable (Three' a) where
    foldMap :: Monoid m => (a2 -> m) -> Three' a1 a2 -> m
    foldMap f (Three' a b b') = f b <> f b'

    foldr :: (a2 -> b -> b) -> b -> Three' a1 a2 -> b
    foldr f b (Three' a b' _) = f b' b
  
  instance Traversable (Three' a) where

    traverse :: Applicative f => (a2 -> f b) -> Three' a1 a2 -> f (Three' a1 b)
    traverse f (Three' a b b') = liftA2 (Three' a) (f b) (f b')
  
  instance (Arbitrary a, Arbitrary b) => Arbitrary (Three' a b) where
    arbitrary = do
      a <- arbitrary
      b <- arbitrary
      return (Three' a b b)

  instance (Eq a, Eq b) => EqProp (Three' a b) where
    (=-=) = eq

  data S n a =
    S (n a) a
    deriving (Eq, Show)

  -- to make it easier, we'll give you the constraints.

  instance Functor n => Functor (S n) where
    fmap :: (a -> b) -> S n a -> S n b
    fmap f (S n a) = S n' b where
                     b  = f a
                     n' = fmap f n

  instance Foldable n => Foldable (S n) where

    foldMap :: (Foldable n, Monoid m) => (a -> m) -> S n a -> m
    foldMap f (S n a) = foldMap f n <> f a

    foldr :: Foldable n => (a -> b -> b) -> b -> S n a -> b
    foldr f b (S n a) = f a (foldr f b n)

  instance Traversable n => Traversable (S n) where
    traverse :: (Traversable n, Applicative f) => (a -> f b) -> S n a -> f (S n b)
    traverse f (S n a) = liftA2 S (traverse f n) b where
                         b = f a



  instance (Arbitrary a, Arbitrary (n a)) => Arbitrary (S n a) where
    arbitrary = do
      liftA2 S arbitrary arbitrary


  instance (Eq a, Eq (n a)) => EqProp (S n a) where
    (=-=) = eq

  
  data Tree a =
    Empty
    | Leaf a
    | Node (Tree a) a (Tree a)
    deriving (Eq, Show)
  
  instance Functor Tree where
    fmap :: (a -> b) -> Tree a -> Tree b
    fmap _ Empty    = Empty
    fmap f (Leaf a) = Leaf (f a)
    fmap f (Node t1 a t2) = Node left root right where
                            left = fmap f t1
                            root = f a
                            right = fmap f t2
  
  instance Foldable Tree where
    --foldMap is a bit easier and looks more natural, -- but you can do foldr too for extra credit.
    foldMap :: Monoid m => (a -> m) -> Tree a -> m
    foldMap f Empty = mempty
    foldMap f (Leaf a) = f a
    foldMap f (Node left a right) = foldMap f left <> f a <> foldMap f right

    foldr :: (a -> b -> b) -> b -> Tree a -> b
    foldr _ b Empty = b
    foldr f b (Leaf a) = f a b
    foldr f b (Node left a right) = f a (foldr f foldedRightTreeResult left) where
      --acc = f a b
      foldedRightTreeResult = foldr f b right
  
  instance Traversable Tree where

    traverse :: Applicative f => (a -> f b) -> Tree a -> f (Tree b)
    traverse _ Empty               = pure Empty
    traverse f (Leaf a)            = fmap Leaf (f a)
    traverse f (Node t1 a t2) = liftA3 Node left root right where
                                left = traverse f t1
                                right = traverse f t2
                                root = f a

  instance (Arbitrary a) => Arbitrary (Tree a) where
    arbitrary = oneof [
        return Empty,
        Leaf <$> arbitrary,
        Node <$> arbitrary <*> arbitrary <*> arbitrary
      ]

  instance Eq a => EqProp (Tree a) where
    (=-=) = eq

  type Appl = Either String String

  exampleList :: List (Appl, Appl, String, String)
  exampleList = Cons testType (Cons testType Nil)

  testType = undefined :: (Appl, Appl, String, String)

  main = do
    quickBatch (traversable (Identity testType))
    quickBatch (traversable (undefined :: Constant (Appl, Appl, String, String) (Appl, Appl, String, String)))
    quickBatch (traversable (undefined :: Optional (Appl, Appl, String, String)))
    quickBatch (traversable exampleList)
    quickBatch (traversable (undefined :: Three (Appl, Appl, String, String) (Appl, Appl, String, String) (Appl, Appl, String, String)))
    quickBatch (traversable (undefined :: Tree (Appl, Appl, String, String)))
    quickBatch (traversable (undefined :: Three' (Appl, Appl, String, String) (Appl, Appl, String, String)))
    quickBatch (traversable (undefined :: S Maybe (Appl, Appl, String, String)))
