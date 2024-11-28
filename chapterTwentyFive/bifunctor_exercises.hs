{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use bimap" #-}
{-# HLINT ignore "Use first" #-}
{-# HLINT ignore "Use newtype instead of data" #-}
module BifunctorExercises where

{--
  This has nothing to do with anything else in this chapter.
    class Bifunctor p where
    {-# MINIMAL bimap | first, second #-}
    bimap :: (a -> b) -> (c -> d) -> p a c -> p b d bimap f g = first f . second g
    first :: (a -> b) -> p a c -> p b c first f = bimap f id
    second :: (b -> c) -> p a b -> p a c second = bimap id

    It’s a functor that can map over two type arguments instead of just
    one. Write Bifunctor instances for the following types:

    1. The less you think, the easier it’ll be.
    data Deux a b = Deux a b
    2. data Const a b = Const a
    3. data Drei a b c = Drei a b c
    4. data SuperDrei a b c = SuperDrei a b
    5. data SemiDrei a b c = SemiDrei a
    6. data Quadriceps a b c d = Quadzzz a b c d
    7. data Either a b = Left a
    | Right b
--}


  class Bifunctor p where
    {-# MINIMAL bimap | first, second #-}

    bimap :: (a -> b) -> (c -> d) -> p a c -> p b d
    bimap f g = first f . second g

    first :: (a -> b) -> p a c -> p b c
    first f = bimap f id
    
    second :: (b -> c) -> p a b -> p a c
    second = bimap id

--1. The less you think, the easier it’ll be.
  data Deux a b =
    Deux a b

  instance Bifunctor Deux where

    bimap :: (a -> b) -> (c -> d) -> Deux a c -> Deux b d
    bimap f g (Deux a c) = Deux (f a) (g c)

--2. data Const a b = Const a

  data Const a b =
    Const a

  instance Bifunctor Const where
    bimap :: (a -> b) -> (c -> d) -> Const a c -> Const b d
    bimap f _ (Const a) = Const (f a)

--3. data Drei a b c = Drei a b c

  data Drei a b c =
    Drei a b c
    
  instance Bifunctor (Drei a) where

    bimap :: (a2 -> b) -> (c -> d) -> Drei a1 a2 c -> Drei a1 b d
    bimap f g (Drei a b c) = Drei a (f b) (g c)

-- 4. data SuperDrei a b c = SuperDrei a b

  data SuperDrei a b c =
    SuperDrei a b

  instance Bifunctor (SuperDrei a) where

    bimap :: (a2 -> b) -> (c -> d) -> SuperDrei a1 a2 c -> SuperDrei a1 b d
    bimap f _ (SuperDrei a b) = SuperDrei a (f b)

-- 5. data SemiDrei a b c = SemiDrei a

  data SemiDrei a b c =
    SemiDrei a

  instance Bifunctor (SemiDrei a) where
    bimap :: (a2 -> b) -> (c -> d) -> SemiDrei a1 a2 c -> SemiDrei a1 b d
    bimap _ _ (SemiDrei a) = SemiDrei a

-- 6. data Quadriceps a b c d = Quadzzz a b c d

  data Quadriceps a b c d =
    Quadzzz a b c d

  instance Bifunctor (Quadriceps a b) where
    bimap :: (a2 -> b2) -> (c -> d) -> Quadriceps a1 b1 a2 c -> Quadriceps a1 b1 b2 d
    bimap f g (Quadzzz a b c d) = Quadzzz a b (f c) (g d)  

-- 7. data Either a b = Left a | Right b

  data Either' a b =
    Left' a
    | Right' b

  instance Bifunctor Either' where
    bimap :: (a -> b) -> (c -> d) -> Either' a c -> Either' b d
    bimap f _ (Left' a) = Left' (f a)
    bimap _ g (Right' b) = Right' (g b)


