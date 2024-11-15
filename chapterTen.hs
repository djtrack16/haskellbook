module ChapterTen where

  import Data.Time

  data DatabaseItem = DbString String
                    | DbNumber Integer
                    | DbDate   UTCTime
    deriving (Eq, Ord, Show)


  {--foldright = foldr (\x y -> concat ["(",x,"+",y,")"]) "0" (map show [1..5])
  foldleft = foldl (\x y -> concat ["(",x,"+",y,")"]) "0" (map show [1..5])--}

  theDatabase :: [DatabaseItem]
  theDatabase =
    [ DbDate (UTCTime
    (fromGregorian 1911 5 1)
    (secondsToDiffTime 34123)) , DbNumber 9001
    , DbString "Hello, world!"
    , DbDate (UTCTime
                (fromGregorian 1921 5 1)
                (secondsToDiffTime 34123))
    ]

  filterDbDate :: [DatabaseItem] -> [UTCTime]
  filterDbDate [] = []
  filterDbDate ((DbDate time):xs) = time : filterDbDate xs

  filterDbNumber :: [DatabaseItem] -> [Integer]
  filterDbNumber [] = []
  filterDbNumber ((DbNumber num):xs) = num : filterDbNumber xs


  mostRecent :: [DatabaseItem] -> UTCTime
  mostRecent items = foldr max time (filterDbDate items)
    where time = UTCTime
                  (fromGregorian 1000 5 1)
                  (secondsToDiffTime 34123)


  sumDb :: [DatabaseItem] -> Integer
  sumDb = sum . filterDbNumber

  avgDb :: [DatabaseItem] -> Double
  avgDb items = let total = sumDb items
                    size = length $ filterDbNumber items
                in fromIntegral  total / fromIntegral size

  -- nth fibonacci
  fibs = 1 : scanl (+) 1 fibs
  fibsN x = fibs !! x
  firstNFibs n = take n fibs -- take 20 
  firstFibsLessThanN n = filter (< n) (take 200 fibs)

  -- factorial as scanl

  factorial = scanl (*) 1 [1..]
{--
  data MyType = MyInt Int | MyChar Char

  foo :: MyType -> MyType
  foo (MyInt i) = MyInt (i + 1) -- Or whatever
  foo (MyChar c) = case c of {'a' -> MyChar 'A'; k -> MyChar k}
--}

-- 10.10

  threeTuples = let stops = "pbtdkg"
                    vowels = "aeiou"
                in [ ( s, v, s ) | s <- stops, v <- vowels ]

  threeTuplesStartsWith char = filter (\(x, _, _) -> x == char) threeTuples

  seekritFunc x = sum $ map length $ words x -- / length (words x)

  -- point free version 
  myOr :: [Bool] -> Bool
  myOr = foldr (||) False

  myAny :: (a -> Bool) -> [a] -> Bool
  myAny f = foldr (\x y -> f x || y) False

  myElem :: Eq a => a -> [a] -> Bool
  myElem x = myAny (==x)

  myReverse :: [a] -> [a]
  myReverse = reverse . foldr (:) []

  myMap :: (a -> b) -> [a] -> [b]
  myMap f = foldr ((:) . f) []

  myFilter :: (a -> Bool) -> [a] -> [a]
  myFilter f = foldr (\a b -> if f a then a : b else b) []

  squish :: [[a]] -> [a]
  squish = foldr (++) []

  squishMap :: (a -> [b]) -> [a] -> [b]
  squishMap f xs = foldr (++) [] $  map f xs

  myMaximumBy :: (a -> a -> Ordering) -> [a] -> a
  myMaximumBy = undefined--foldr (\a b -> )