{-# OPTIONS_GHC -Wno-noncanonical-monoid-instances #-}
{-# OPTIONS_GHC -Wno-overlapping-patterns #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Monoid law, left identity" #-}
{-# HLINT ignore "Monoid law, right identity" #-}
{-# HLINT ignore "Use <$>" #-}
module ChapterFifteen where

  import Data.Monoid
  import Control.Monad
  import Test.QuickCheck
{--
-- left identity
mappend mempty x = x
-- right identity
mappend x mempty = x
-- associativity
mappend x (mappend y z) = mappend (mappend x y) z
mconcat = foldr mappend mempty

Expected output:
Prelude> Only (Sum 1) `mappend` Only (Sum 1)
Only (Sum {getSum = 2})

Prelude> Only (Product 4) `mappend` Only (Product 2)
Only (Product {getProduct = 8})

Prelude> Only (Sum 1) `mappend` Nada
Only (Sum {getSum = 1})

Prelude> Only [1] `mappend` Nada
Only [1]

Prelude> Nada `mappend` Only (Sum 1)
Only (Sum {getSum = 1})
--}

  data Optional a =
    Nada
    | Only a
    deriving (Eq, Show)

  instance Monoid a => Monoid (Optional a) where
    mempty :: Monoid a => Optional a
    mempty = Nada

  instance Semigroup a => Semigroup (Optional a) where
    -- left and right identity, x can be Nada or Only a, etc
    (<>) :: Semigroup a => Optional a -> Optional a -> Optional a
    (<>) x Nada = x
    (<>) Nada y = y
    (<>) (Only x) (Only y) = Only (x <> y)

  -- generic associativity function for arbitrary types
  -- for any function in the abstract, and any type in the abstract
  associativity :: Eq a => (a -> a -> a) -> a -> a -> a -> Bool
  associativity (<>) x y z = x <> (y <> z) == (x <> y) <> z

  -- for monoids and appends in particular
  monoidAssoc :: (Eq m, Monoid m) => m -> m -> m -> Bool
  monoidAssoc x y z = x <> (y <> z) == (x <> y) <> z

  monoidLeftIdentity :: (Eq m, Monoid m) => m -> Bool
  monoidLeftIdentity a = (mempty <> a) == a

  monoidRightIdentity :: (Eq m, Monoid m) => m -> Bool
  monoidRightIdentity a = (a <> mempty) == a

  newtype First' a =
    First' { getFirst' :: Optional a }
    deriving (Eq, Show)

 -- instance Arbitrary a => Arbitrary (First' a) where
   -- arbitrary = First' <$> arbitrary <*> arbitrary

  instance Semigroup a => Semigroup (First' a) where

    (<>) :: Semigroup a => First' a -> First' a -> First' a
    (<>) x (First' Nada) = x
    (<>) (First' Nada) y = y
    (<>) (First' (Only x)) _ = First' (Only x)

  --firstMappend :: Semigroup a => First' a -> First' a -> First' a
  --firstMappend = (<>)

  main :: IO ()
  main = do
    --quickCheck (monoidAssoc :: FirstMappend)
    --quickCheck (monoidLeftIdentity :: First' String -> Bool)
    --quickCheck (monoidRightIdentity :: First' String -> Bool)
    putStrLn ""
