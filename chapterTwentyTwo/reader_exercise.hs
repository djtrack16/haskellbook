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

  newtype HumanName = HumanName String deriving (Eq, Show)
  newtype DogName = DogName String deriving (Eq, Show)

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

--1. Write liftA2 yourself. Think about it in terms of abstracting
--out the difference between getDogR and getDogR' if that helps.

  myLiftA2 :: Applicative f => (a -> b -> c) -> f a -> f b -> f c
  myLiftA2 f fa fb = undefined

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