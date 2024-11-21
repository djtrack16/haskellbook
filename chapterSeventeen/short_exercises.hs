module ShortExercises where

  import Data.Functor
  import Control.Applicative
  import Data.List (elemIndex)


  -- starting p 663
{--
In the following exercises you will need to use the following terms to make the expressions type-check:
1. pure
2. (<$>)
-- or fmap
3. (<*>)
Make the following expressions type-check.

1. added :: Maybe Integer
added = (+3) (lookup 3 $ zip [1, 2, 3] [4, 5, 6])

2. y :: Maybe Integer
y = lookup 3 $ zip [1, 2, 3] [4, 5, 6]
z :: Maybe Integer
z = lookup 2 $ zip [1, 2, 3] [4, 5, 6]
tupled :: Maybe (Integer, Integer) tupled = (,) y z

3. import Data.List (elemIndex) x :: Maybe Int
x = elemIndex 3 [1, 2, 3, 4, 5] y :: Maybe Int
y = elemIndex 4 [1, 2, 3, 4, 5] max' :: Int -> Int -> Int
max' = max
maxed :: Maybe Int maxed = max' x y

4. xs = [1, 2, 3]
ys = [4, 5, 6]
x :: Maybe Integer
x = lookup 3 $ zip xs ys y :: Maybe Integer
y = lookup 2 $ zip xs ys summed :: Maybe Integer
summed = sum $ (,) x y
--}

  -- #1
  added :: Maybe Integer
  added = fmap (+3) (lookup 3 $ zip [1, 2, 3] [4, 5, 6])

  -- #2

  y :: Maybe Integer
  y = lookup 3 $ zip [1, 2, 3] [4, 5, 6]

  z :: Maybe Integer
  z = lookup 2 $ zip [1, 2, 3] [4, 5, 6]

  tupled :: Maybe (Integer, Integer)
  tupled = (,) <$> y <*> z

  -- #3
  x :: Maybe Int
  x = elemIndex 3 [1, 2, 3, 4, 5]
  
  x' :: Maybe Int
  x' = elemIndex 4 [1, 2, 3, 4, 5]
  
  max' :: Int -> Int -> Int
  max' = max

  maxed :: Maybe Int
  maxed = max' <$> x <*> x'

  -- #4
  xs = [1, 2, 3]
  ys = [4, 5, 6]

  val1 :: Maybe Integer
  val1 = lookup 3 $ zip xs ys
  
  val2 :: Maybe Integer
  val2 = lookup 2 $ zip xs ys
  
  summed :: Maybe Integer
  summed = fmap sum $ (,) <$> val1 <*> val2

-- Write an Applicative instance for Identity.
  newtype Identity a =
    Identity a
    deriving (Eq, Ord, Show)

  instance Functor Identity where

    fmap :: (a -> b) -> Identity a -> Identity b
    fmap f (Identity a) = Identity (f a)
  
  instance Applicative Identity where

    pure :: a -> Identity a
    pure = Identity

    (<*>) :: Identity (a -> b) -> Identity a -> Identity b
    (<*>) (Identity f) (Identity a') = Identity (f a')

-- Write an Applicative instance for Constant.
  newtype Constant a b =
    Constant { getConstant :: a }
    deriving (Eq, Ord, Show)

  instance Functor (Constant a) where

    fmap :: (a2 -> b) -> Constant a1 a2 -> Constant a1 b
    fmap _ (Constant a1) = Constant a1

  instance Monoid a => Applicative (Constant a) where
    
    pure :: Monoid a => b -> Constant a b
    pure b = Constant mempty
    
    (<*>) :: Monoid a => Constant a (a1 -> b) -> Constant a a1 -> Constant a b
    (<*>) (Constant a) (Constant a') = Constant (a <> a') 
