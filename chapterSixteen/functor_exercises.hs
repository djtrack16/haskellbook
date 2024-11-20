{-# LANGUAGE FlexibleInstances #-}

module FunctorExercise where

  import GHC.Arr
  import Data.Functor qualified as F
  -- start from page 643
{--
  Determine if a valid Functor can be written for the datatype provided.
  1. data Bool =
  False | True
  2. data BoolAndSomethingElse a =
  False' a | True' a
  3. data BoolAndMaybeSomethingElse a =
  Falsish | Truish a
  4. Use the kinds to guide you on this one, don’t get too hung up on
  the details.
  newtype Mu f = InF { outF :: f (Mu f) }
  5. Again, just follow the kinds and ignore the unfamiliar parts
  import GHC.Arr data D =
  D (Array Word Word) Int Int
--}

  data MyBool =
    MyFalse
    | MyTrue
  -- Not possible because the kind is wrong aka
  -- Expected kind ‘* -> *’, but ‘MyBool’ has kind ‘*’
  -- For example 'data MyBool a' would have the correct kind even if the argument was unused
  -- in the constructors...
  --
  --instance Functor MyBool where
  --  fmap = undefined

  data BoolAndSomethingElse a =
    False' a |
    True' a

  instance Functor BoolAndSomethingElse where
    fmap f (False' a) = False' (f a)
    fmap f (True' a)  = True' (f a)
  
  data BoolAndMaybeSomethingElse a =
    Falsish
    | Truish a

  instance Functor BoolAndMaybeSomethingElse where
    fmap _ Falsish     = Falsish
    fmap f (Truish a)  = Truish (f a)
{--
  newtype Mu f = InF
    { outF :: f (Mu f) }

  instance Functor (Mu g) where
    fmap f (InF a b) = Inf (f a) (InF $ f b)

  data D =
    D (Array Word Word) Int Int

  instance Functor D where
    fmap f (D tuple x y) = D tuple x (f y)

--}
  -- Rearrange the arguments to the type constructor of the datatype so the Functor instance works.
  -- Two Solutions here:
  -- 1. Change "data Sum a b" to "data Sum b a" OR
  -- 2. Use "Second (f b)" instead of "Second b" and unapply f from "First a"
  data Sum a b  =
    First a
    | Second b

  instance Functor (Sum e) where
    fmap _ (First a) = First a
    fmap f (Second b) = Second (f b)

  
  -- Two Possible Solutions here:
  -- 1. Change "Company a b c" to "Company a c b" OR
  -- 2. Use "DeepBlue a c" instead of "DeepBlue a (f c)" and unapply f from "Something b"
  data Company a c b =
    DeepBlue a c
    | Something b

  instance Functor (Company e e') where
    fmap f (Something b) = Something (f b)
    fmap _ (DeepBlue a c) = DeepBlue a c



