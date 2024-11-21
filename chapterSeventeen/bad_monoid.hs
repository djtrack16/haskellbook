module BadMonoid where
  import Data.Monoid qualified as M
  import Data.Semigroup qualified as S
  import Test.QuickCheck
  import Test.QuickCheck.Checkers
  import Test.QuickCheck.Classes
  import Control.Applicative

  type Sum = S.Sum

  data Bull =
    Fools
    | Twoo
    deriving (Eq, Show)

  instance Arbitrary Bull where
    arbitrary =
      frequency [ (1, return Fools) , (1, return Twoo) ]

  instance Monoid Bull where
    mempty = Fools

  instance S.Semigroup Bull where
    _ <> _ = Fools

  --instance EqProp Bull where
  --  (=-=) = eq

-- ZIPLIST MONOID

  instance Monoid a => Monoid (ZipList a) where
    mempty = pure mempty

  instance Semigroup a => S.Semigroup (ZipList a) where
    (<>) = liftA2 (S.<>)



  --instance Arbitrary a => Arbitrary (ZipList a) where
  --  arbitrary = ZipList <$> arbitrary

  --instance Arbitrary a => Arbitrary (Sum a) where
   -- arbitrary = Sum <$> arbitrary

  --instance Eq a => EqProp (ZipList a) where
    --(=-=) = eq

{--
Implement the List Applicative. Writing a minimally complete Ap- plicative instance calls for
writing the definitions of both pure and <*>. We’re going to provide a hint as well.
Use the checkers library to validate your Applicative instance.

Prelude> let functions = Cons (+1) (Cons (*2) Nil)
Prelude> let values = Cons 1 (Cons 2 Nil)
Prelude> functions <*> values
Cons 2 (Cons 3 (Cons 2 (Cons 4 Nil)))

    (<*>) :: Identity (a -> b) -> Identity a -> Identity b
    (<*>) (Identity f) (Identity a') = Identity (f a')

--}

  data List a =
    Nil
    | Cons a (List a)
    deriving (Eq, Show)

  instance Functor List where
    fmap = undefined

  instance Applicative List where
    pure :: a -> f a
    pure = undefined--f

    (<*>) :: List (a -> b) -> List a -> List b
    (<*>) Nil Nil  = Nil
    --(<*>) (Cons a list) Nil  = Cons a Nil <*> list

    --(<*>) f (Cons a list) = Cons (f a) (f <*> list)

  main :: IO ()
  main = do
    undefined
    --quickBatch (monoid Twoo)
    --quickBatch $ monoid (ZipList [1 :: Sum Int])
