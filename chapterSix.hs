{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Eta reduce" #-}
{-# HLINT ignore "Use newtype instead of data" #-}

module ChapterSix where

  main :: IO()
  main = do
    print (1 + 2)
    print 10
    print (negate (-1))
    print ((+) 0 blah)
      where blah = negate 1


  f :: Int -> String
  f = undefined

  g :: String -> Char
  g = undefined

  h :: Int -> Char
  h = g . f

  data A
  data B
  data C

  q :: A -> B
  q = undefined

  w :: B -> C
  w = undefined

  e :: A -> C
  e = w . q

  data X
  data Y
  data Z

  xz :: X -> Z
  xz = undefined

  yz :: Y -> Z
  yz = undefined

  xform :: (X, Y) -> (Z, Z)
  xform (x, y) = (xz x, yz y)

  munge :: (x -> y) -> (y -> (w, z)) -> x -> w
  munge f g x = fst $ g $ f x

  data Person = Person Bool deriving Show

  printPerson :: Person -> IO ()
  printPerson person = print person


  data Mood = Blah
            | Woot deriving(Show, Eq, Ord)
  settleDown x = if x == Woot then Blah else x
  order x = if x > Woot then Blah else x

  type Subject = String
  type Verb = String
  type Object = String

  data Sentence = Sentence Subject Verb Object deriving (Eq, Show)
  s1 = Sentence "dogs" "drool"
  s2 = Sentence "Julie" "loves" "dogs"

  data Rocks =
    Rocks String deriving (Show, Eq)
  data Yeah =
    Yeah Bool deriving (Show, Eq)
  data Papu =
    Papu Rocks Yeah deriving Eq

  truth = Papu (Rocks "chomskydoz")
    (Yeah True)

  equalityForall :: Papu -> Papu -> Bool
  equalityForall p p' = p == p'

  chk :: Eq b => (a -> b) -> a -> b -> Bool
  chk f x y = f x == y

  arith :: Num b => (a -> b) -> Integer -> a -> b
  arith f x y = f y + fromInteger x