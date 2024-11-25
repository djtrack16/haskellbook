{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use <&>" #-}
{-# HLINT ignore "Eta reduce" #-}
{-# HLINT ignore "Use map" #-}
{-# HLINT ignore "Redundant bracket" #-}
{-# HLINT ignore "Use foldr" #-}
module MiscExercises where

  import Control.Monad
  import Data.Functor
  import Data.Monoid
  import Control.Applicative

{--
Write the following functions using the methods provided by Monad and Functor. Using stuff like identity and composition are fine, but it has to typecheck with types provided.
1. j :: Monad m => m (m a) -> m a
    Expecting the following behavior:
    Prelude> j [[1, 2], [], [3]]
    [1,2,3]
    Prelude> j (Just (Just 1))
    Just 1
    Prelude> j (Just Nothing)
    Nothing
    Prelude> j Nothing
    Nothing
2. l1 :: Monad m => (a -> b) -> m a -> m b
3. l2 :: Monad m => (a -> b -> c) -> m a -> m b -> m c
4. a :: Monad m => m a -> m (a -> b) -> m b
5. You’ll need recursion for this one.
meh :: Monad m => [a] -> (a -> m b) -> m [b]
6. Hint: reuse “meh”
flipType :: (Monad m) => [m a] -> m [a]
--}

-- 1. j :: Monad m => m (m a) -> m a

  j :: Monad m => m (m a) -> m a
  j = join

-- 2. l1 :: Monad m => (a -> b) -> m a -> m b

  l1 :: Monad m => (a -> b) -> m a -> m b
  l1 f a = a >>= return . f -- vscode "hint": use <&>.... ok let me google it... 

-- 3. l2 :: Monad m => (a -> b -> c) -> m a -> m b -> m c

  l2 :: Monad m => (a -> b -> c) -> m a -> m b -> m c
  l2 f a b = f <$> a <*> b

-- 4. a :: Monad m => m a -> m (a -> b) -> m b

  a :: Monad m => m a -> m (a -> b) -> m b
  a ma f = f <*> ma

-- 5. You’ll need recursion for this one.

  meh :: Monad m => [a] -> (a -> m b) -> m [b]
  meh []     _ = return []
  meh (a:as) f = let first = (:[]) <$> f a
                     rest  = meh as f
                 in first >> rest


-- 6. Hint: reuse “meh”
  flipType :: Monad m => [m a] -> m [a]
  flipType mas = undefined --(fmap . fmap) (+1) mas

  --(>=>) :: Monad m => (a -> m b) -> (b -> m c) -> a -> m c