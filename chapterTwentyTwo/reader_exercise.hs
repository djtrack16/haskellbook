{-# LANGUAGE InstanceSigs #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use <$>" #-}
module ReaderExercise where

  import Control.Applicative

  newtype Reader r a =
    Reader { runReader :: r -> a }

  instance Functor (Reader r) where

    fmap :: (a -> b) -> Reader r a -> Reader r b
    fmap f (Reader ra) = Reader (f . ra)

{--

Implement the following function. If you get stuck, remember it’s less complicated than it looks.
Write down what you know. What do you know about the type 𝑎? What does the type simplify to?
How many inhabitants does that type have? You’ve seen the type before.
--}

  ask :: Reader a a
  ask = Reader id

  newtype HumanName =
    HumanName String
    deriving (Eq, Show)

  newtype DogName =
    DogName String
    deriving (Eq, Show)

  data Person =
    Person {
      humanName :: HumanName
    , dogName :: DogName
    , address :: Address
    }
    deriving (Eq, Show)

  data Dog =
    Dog {
      dogsName :: DogName
    , dogsAddress :: Address
    }
    deriving (Eq, Show)

  newtype Address =
    Address String
    deriving (Eq, Show)

  pers :: Person
  pers = Person (HumanName "Big Bird") (DogName "Barkley") (Address "Sesame Street")

  chris :: Person
  chris = Person (HumanName "Chris Allen") (DogName "Papu") (Address "Austin")


  getDog :: Person -> Dog
  getDog p = Dog (dogName p) (address p)

  -- with Reader
  getDogR :: Person -> Dog
  getDogR = Dog <$> dogName <*> address

  getDogR' :: Reader Person Dog -- after writing Applicative for Reader
  getDogR' = Dog <$> Reader dogName <*> Reader address


  getDogRM :: Person -> Dog
  getDogRM = do
    name <- dogName
    addy <- address
    return $ Dog name addy

  getDogRM' :: Reader Person Dog -- after writing Monad for Reader
  getDogRM' = do
    name <- Reader dogName
    addy <- Reader address
    return $ Dog name addy


--1. Write liftA2 yourself. Think about it in terms of abstracting
--out the difference between getDogR and getDogR' if that helps.

  myLiftA2 :: Applicative f => (a -> b -> c) -> f a -> f b -> f c
  myLiftA2 f fa fb = f <$> fa <*> fb

 --- instance Functor (Reader r) where
  --  fmap :: (a -> b) -> Reader r a -> Reader r b
  --  fmap f (Reader ra) =
  --    Reader $ \r -> f (ra r)

 --   newtype Reader r a =
  --  Reader { runReader :: r -> a }

  asks :: (r -> a) -> Reader r a
  asks = Reader

  instance Applicative (Reader r) where

    pure :: a -> Reader r a
    pure = Reader . pure


    (<*>) :: Reader r (a -> b) -> Reader r a -> Reader r b
    (<*>) (Reader rToab) (Reader rToa) = Reader $ \r -> rToab r $ rToa r

  

{--

4. Rewrite the above example that uses Dog and Person to use your
Reader datatype you just implemented the Applicative for. You’ll need to change the types as well.
--}

  instance Monad (Reader r) where
    
    return = pure

    (>>=) :: Reader r a -> (a -> Reader r b) -> Reader r b
    (>>=) (Reader ra) aToReaderRb = Reader $ \r -> runReader (rrb r) r where
                                    rrb = aToReaderRb . ra

    -- runReader (aToReaderRb (ra r)) r