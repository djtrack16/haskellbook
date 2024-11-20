{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}

module FunctorExercise where

  import GHC.Arr
  import Data.Functor qualified as F
  import Data.List.Class (ListItem())
  import System.IO.Unsafe (unsafeFixIO)
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

{--
  Keeping in mind that it should result in a Functor that does the following:
  Prelude> fmap (+1) (L 1 2 3)
  L 2 2 4
  Prelude> fmap (+1) (R 1 2 3)
  R 1 3 3
--}
-- Solution:
-- Change "More a b" to "More b a" OR
-- Switch places of the "L" and "R" in the data constructor...

  data More a b =
    L b a b
    | R a b a 
    deriving (Eq, Show)
  
  instance Functor (More x) where
    fmap f (L a b a') = L (f a) b (f a')
    fmap f (R b a b') = R b (f a) b'

{--
Write Functor instances for the following datatypes.
1. data Quant a b = Finance
| Desk a | Bloor b
2. No, it’s not interesting by itself.
data K a b = Ka
3. {-# LANGUAGE FlexibleInstances #-}
newtype Flip f a b = Flip (f b a) deriving (Eq, Show)
newtype K a b = Ka
-- should remind you of an
-- instance you've written before instance Functor (Flip K a) where
fmap = undefined
4. data EvilGoateeConst a b =
GoatyConst b
    -- You thought you'd escaped the goats
    -- by now didn't you? Nope.
CHAPTER16. FUNCTOR 645 No, it doesn’t do anything interesting. No magic here or in the
previous exercise. If it works, you succeeded.
5. Do you need something extra to make the instance work?
data LiftItOut f a = LiftItOut (f a)
6. data Parappa f g a = DaWrappa (f a) (g a)
7. Don’t ask for more typeclass instances than you need. You can let GHC tell you what to do.
data IgnoreOne f g a b = IgnoringSomething (f a) (g b)
8. data Notorious g o a t = Notorious (g o) (g a) (g t)
9. You’ll need to use recursion.
data List a = Nil
| Cons a (List a)
10. A tree of goats forms a Goat-Lord, fearsome poly-creature.
data GoatLord a = NoGoat
| OneGoat a
| MoreGoats (GoatLord a) (GoatLord a) (GoatLord a)
    -- A VERITABLE HYDRA OF GOATS
11. You’ll use an extra functor for this one, although your solution might do it monomorphically without using fmap.3
data TalkToMe a = Halt
| Print String a
| Read (String -> a)
--}

  -- #1
  data Quant a b = Finance
    | Desk a
    | Bloor b

  instance Functor (Quant q) where
    fmap _ Finance = Finance
    fmap _ (Desk d) = Desk d
    fmap f (Bloor b) = Bloor (f b)

   -- #2
  newtype K a b =
    K a

  instance Functor (K pair) where
    fmap _ (K a) = K a

  -- #3
  newtype Flip f a b =
    Flip (f b a)
    deriving (Eq, Show)

  --instance Functor f => Functor (Flip f a) where
   -- fmap f' (Flip g) = Flip (fmap f' g)

  -- #4

  data EvilGoateeConst a b =
    GoatyConst b

  instance Functor (EvilGoateeConst ab) where
    fmap f (GoatyConst b) = GoatyConst $ f b
  -- #5

  data LiftItOut f a =
    LiftItOut (f a)

  instance Functor f => Functor (LiftItOut f) where
    fmap f (LiftItOut fa) = LiftItOut (fmap f fa)
  -- #6

  data Parappa f g a =
    DaWrappa (f a) (g a)

  instance (Functor f, Functor g) => Functor (Parappa f g) where
    fmap f (DaWrappa fa ga) = DaWrappa (fmap f fa) (fmap f ga)
  -- #7

  data IgnoreOne f g a b =
    IgnoringSomething (f a) (g b)

  instance Functor g => Functor (IgnoreOne f g a) where
    fmap f (IgnoringSomething fa gb) = IgnoringSomething fa (fmap f gb)
  -- #8

  data Notorious g o a t =
    Notorious (g o) (g a) (g t)

  instance Functor g => Functor (Notorious g o a) where
    fmap :: (a2 -> b) -> Notorious g o a1 a2 -> Notorious g o a1 b
    fmap f (Notorious go ga gt) = Notorious go ga (fmap f gt)
  -- #9

  data List a =
    Nil
    | Cons a (List a)

  instance Functor List where
    fmap _ Nil = Nil
    fmap f (Cons a list) = Cons (f a) (fmap f list)
  -- #10
  data GoatLord a =
    NoGoat
    | OneGoat a
    | MoreGoats (GoatLord a) (GoatLord a) (GoatLord a)

  instance Functor GoatLord where
    fmap :: (a -> b) -> GoatLord a -> GoatLord b
    fmap _ NoGoat = NoGoat
    fmap f (OneGoat a) = OneGoat (f a)
    fmap f (MoreGoats g1 g2 g3) = MoreGoats (fmap f g1) (fmap f g2) (fmap f g3)
  -- #11