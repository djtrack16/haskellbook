{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use <$>" #-}
{-# LANGUAGE MonoLocalBinds #-}
module List where

  import Data.Monoid qualified as M
  import Data.Semigroup qualified as S

  import Test.QuickCheck
  import Test.QuickCheck.Checkers
  import Test.QuickCheck.Classes
  import Control.Applicative

  take' :: Int -> List a -> List a
  take' = undefined

  -- ZIPLIST MONOID

  newtype ZipList' a =
    ZipList' (List a)
    deriving (Eq, Show)

  instance Eq a => EqProp (ZipList' a) where
    xs =-= ys = xs' `eq` ys'
      where xs' = let (ZipList' l) = xs
                  in take' 3000 l
            ys' = let (ZipList' l) = ys
                  in take' 3000 l

  instance Functor ZipList' where
    fmap :: (a -> b) -> ZipList' a -> ZipList' b
    fmap f (ZipList' xs) = ZipList' (fmap f xs)

  instance Applicative ZipList' where

    pure :: a -> ZipList' a
    pure a = ZipList' (Cons a Nil)


    (<*>) :: ZipList' (a -> b) -> ZipList' a -> ZipList' b
    (<*>) (ZipList' Nil) (ZipList' list) = ZipList' Nil
    (<*>) (ZipList' list) (ZipList' Nil) = ZipList' Nil
    (<*>) (ZipList' (Cons f list)) (ZipList' (Cons a list')) = ZipList' (Cons (f a) (list <*> list'))



  instance (Arbitrary a) => Arbitrary (ZipList (List a)) where
    arbitrary = do
      a <- arbitrary
      frequency [
        (1, return (ZipList' Nil)) ,
        (1, return ZipList' (Cons <$> arbitrary <*> arbitrary))
        ]

{--

Implement the List Applicative. Writing a minimally complete Ap- plicative instance calls for
writing the definitions of both pure and <*>. We’re going to provide a hint as well.
Use the checkers library to validate your Applicative instance.

Prelude> let functions = Cons (+1) (Cons (*2) Nil)
Prelude> let values = Cons 1 (Cons 2 Nil)
Prelude> functions <*> values
Cons 2 (Cons 3 (Cons 2 (Cons 4 Nil)))


--}

  data List a =
    Nil
    | Cons a (List a)
    deriving (Eq, Show)

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

  instance (Arbitrary a) => Arbitrary (List a) where
    arbitrary :: Arbitrary a => Gen (List a)
    arbitrary = oneof [
        return Nil,
        Cons <$> arbitrary <*> arbitrary
      ]

  instance Eq a => EqProp (List a) where
    (=-=) = eq

  functions = Cons (+1) (Cons (*2) Nil)
  values = Cons 1 (Cons 2 Nil)

  testType = undefined :: (String, String, String)


  append :: List a -> List a -> List a
  append Nil ys = ys
  append (Cons x xs) ys = Cons x $ xs `append` ys

  fold :: (a -> b -> b) -> b -> List a -> b
  fold _ b Nil = b
  fold f b (Cons h t) = f h (fold f b t)

  concat' :: List (List a) -> List a
  concat' = fold append Nil

  -- write this one in terms of concat' and fmap
  flatMap :: (a -> List b) -> List a -> List b
  flatMap f as = concat' $ fmap f as

  exampleList :: List (String, String, String)
  exampleList = Cons testType (Cons testType Nil)
  

  main :: IO()
  main = do
    quickBatch (applicative exampleList)
    quickBatch (applicative (ZipList' exampleList))
