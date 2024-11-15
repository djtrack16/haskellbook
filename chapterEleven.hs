{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}

module ChapterEleven where
  import Data.Foldable (maximumBy)
  import qualified Data.List as L

  data Doggies a =
    Husky a
    | Mastiff a
    deriving (Eq, Show)

  data DogueDeBordeaux doge = DogueDeBordeaux doge

  data Price =
    Price Integer deriving (Eq, Show)

  data Size =
    Size Integer deriving (Eq, Show)

  data Manufacturer =
    Mini
    | Mazda
    | Tata
    deriving (Eq, Show)

  data Airline =
    PapuAir
    | CatapultsR'Us
    | TakeYourChancesUnited
    deriving (Eq, Show)

  data Vehicle =
    Car Manufacturer Price
    | Plane Airline Size
    deriving (Eq, Show)

  myCar = Car Mini (Price 14000)
  urCar = Car Mazda (Price 20000)
  clownCar = Car Tata (Price 7000)
  doge = Plane PapuAir

{--
1. data constructor
2. Doggies :: * -> *
3. Doggies String :: *
4. Doggies (Num a => Doggies a)
5. Husky 10 :: Doggies Integer
6. Doggies (String => Doggies a)
....

--}

  isCar :: Vehicle -> Bool
  isCar (Car _ _) = True
  isCar (Plane _ _) = False

  isPlane :: Vehicle -> Bool
  isPlane v = not (isCar v)

  areCars :: [Vehicle] -> [Bool]
  areCars = map isCar

  getManu :: Vehicle -> Maybe Manufacturer
  getManu (Car manu _) = Just manu
  getManu (Plane _ _) = Nothing
{--
  class TooMany a where
    tooMany :: a -> Bool
  
  instance TooMany (Int, String) where
    tooMany :: (Int, String) -> Bool
    tooMany (n, _) = n > 42

  newtype Goats = Goats Int deriving (Eq, Show, TooMany)

  instance TooMany (Int, Int) where
    tooMany :: (Int, Int) -> Bool
    tooMany (m, n) = m + n > 84

 -- instance TooMany (Int, Int) where
  --  tooMany :: (Num a, TooMany a) => (a, a) -> Bool
  --  tooMany (m,n) = (m + n > 84)

  --instance TooMany (Num a, TooMany a) => (a, a) where
  --  tooMany n tm = if tm then n > 1 else n == 0
--}

  data Example a = MakeExample deriving Show

  allJams = [
    Jam {fruit = Peach, jars = 5},
    Jam {fruit = Peach, jars = 3},
    Jam {fruit = Plum, jars = 8},
    Jam {fruit = Plum, jars = 4},
    Jam {fruit = Apple, jars = 10},
    Jam {fruit = Blackberry, jars = 7},
    Jam {fruit = Blackberry, jars = 4}
    ]

  data Fruit =
    Peach
    | Plum
    | Apple
    | Blackberry deriving (Eq, Show, Ord)

  data JamJars = Jam
    {
      fruit :: Fruit,
      jars :: Int
    } deriving (Eq, Show, Ord)
  
  compareKind :: JamJars -> JamJars -> Ordering
  compareKind (Jam k _) (Jam k' _) = compare k k'

  compareJars :: JamJars -> JamJars -> Ordering
  compareJars (Jam _ j1) (Jam _ j2) = compare j1 j2

  totalJars :: Int
  totalJars = sum $ map jars allJams

  maxJamInRow :: Int
  maxJamInRow = jars $ maximumBy compareJars allJams

  sortedJamsByFruit :: [JamJars]
  sortedJamsByFruit = L.sortBy compareKind allJams

  groupedJamsByFruit :: [[JamJars]]
  groupedJamsByFruit = L.groupBy (\f f' -> fruit f == fruit f') allJams

