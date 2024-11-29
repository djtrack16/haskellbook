{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use and" #-}
module ReaderPractice where
  import Control.Applicative
  import Data.Maybe

  x = [1, 2, 3]
  y = [4, 5, 6]
  z = [7, 8, 9]
 -- The next thing we want to do is write some functions that zip those lists together
 -- and uses lookup to find the value associated with a specified key in our zipped lists.
 -- For demonstration purposes, it’s nice to have the outputs be predictable,
 -- so we recommend writing some that are concrete values, as well as one that can be applied to a variable:
  --lookup :: Eq a => a -> [(a, b)] -> Maybe b

  -- zip x and y using 3 as the lookup key
  xs :: Maybe Integer
  xs = lookup 3 $ zip x y

  -- zip y and z using 6 as the lookup key
  ys :: Maybe Integer
  ys = lookup 6 $ zip y z

  zs :: Maybe Integer
  zs = lookup 4 $ zip x y

  z' :: Integer -> Maybe Integer
  z' n = lookup n $ zip x z

{--
Now we want to add the ability to make a Maybe (,) of values using Applicative.
Have x1 make a tuple of xs and ys, and x2 make a tuple of of ys and zs.
Also, write x3 which takes one input and makes a tuple of the results of two applications of z' from above.
--}

  x1 :: Maybe (Integer, Integer)
  x1 = liftA2 (,) xs ys

  x2 :: Maybe (Integer, Integer)
  x2 = fmap (,) ys <*> zs

  x3 :: Integer -> (Maybe Integer, Maybe Integer)
  x3 = liftA2 (,) z' z'

  summed :: Num c => (c, c) -> c
  summed = uncurry (+)

  -- use &&, >3, <8
  -- And now we’ll make a function similar to some we’ve seen before that lifts a
  -- boolean function over two partially-applied functions:
  bolt :: Integer -> Bool 
  bolt = liftA2 (&&) (>3) (<8)

  sequA :: Integral a => a -> [Bool]
  sequA = sequenceA [(>3), (<8), even]

  s' = fmap summed $ (,) <$> xs <*> ys

  main :: IO ()
  main = do
    print $ sequenceA [Just 3, Just 2, Just 1]
    print $ sequenceA [x, y]
    print $ sequenceA [xs, ys]
    print $ summed <$> ((,) <$> xs <*> ys)
    print $ fmap summed ((,) <$> xs <*> zs)
    print $ bolt 7
    print $ fmap bolt z
    print $ foldr (&&) True $ sequA 5
    print $ sequA (fromMaybe 3 s')
    print $ bolt (fromMaybe 4 ys)
    print $ bolt (fromMaybe 6 $ z' 6)
