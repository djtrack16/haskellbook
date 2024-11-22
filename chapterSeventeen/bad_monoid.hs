{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Eta reduce" #-}
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

  main :: IO ()
  main = do
    undefined
    --quickBatch (monoid Twoo)
    --quickBatch $ monoid (ZipList [1 :: Sum Int])
