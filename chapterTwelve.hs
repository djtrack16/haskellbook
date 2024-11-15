module ChapterTwelve where

  import Data.Maybe hiding (isJust, catMaybes, listToMaybe)
  import Data.String
  import Data.Char (toLower)
  import Foreign (maybeNew)
  --import Data.List (intercalate)

  notThe :: String -> Maybe String
  notThe "the" = Nothing
  notThe str = Just str

  replaceThe :: String -> String
  replaceThe text = unwords $ map f $ words text where
    f w = Data.Maybe.fromMaybe "a" (notThe w)

  countVowels :: String -> Int
  countVowels word = length (filter isVowel word)

  isVowel :: Char -> Bool
  isVowel c = toLower c `elem` ['a', 'e', 'i', 'o', 'u']


  newtype Word' =
    Word' String deriving (Eq, Show)

  mkWord :: String -> Maybe Word'
  mkWord text = let
      allWords = words text
      charCount = length text - length allWords + 1 -- subtract whitespace characters
      numVowels = sum $ map countVowels allWords
      numConsonants = charCount - numVowels
    in if numVowels > numConsonants then Nothing else Just (Word' text)

  -- also the basic example from PLFA foundations in Agda :P 
  data Nat =
    Zero
    | Succ Nat
    deriving (Eq, Show)

  natToInteger :: Nat -> Integer
  natToInteger Zero = 0
  natToInteger (Succ nat) = 1 + natToInteger nat

  integerToNat :: Integer -> Maybe Nat
  integerToNat 0 = Just Zero
  integerToNat n = if n < 0 then Nothing else Just nextNat where
    nextNat = Succ $ fromJust $ integerToNat (n-1)

  -- Small library for Maybe


  isJust' :: Maybe a -> Bool
  isJust' Nothing = False
  isJust' _ = True

  isNothing :: Maybe a -> Bool
  isNothing a = not $ isJust' a

  -- >>> mayybee 0 (+1) Nothing
  -- 0
  -- >>> mayybee 0 (+1) (Just 1)
  -- 2
  mayybee :: b -> (a -> b) -> Maybe a -> b
  mayybee defaultVal _ Nothing    = defaultVal
  mayybee _ f (Just val)          = f val

  -- >>> fromMaybe 0 Nothing
  -- 0
  -- >>> fromMaybe 0 (Just 1)
  -- 1
  fromMaybe :: a -> Maybe a -> a
  fromMaybe b Nothing = b
  fromMaybe b (Just val) = val

  -- >>> listToMaybe [1, 2, 3]
  -- Just 1
  -- >>> listToMaybe []
  -- Nothing
  listToMaybe :: [a] -> Maybe a
  listToMaybe (x:xs) = Just x
  listToMaybe [] = Nothing
    -- >>> maybeToList (Just 1)
    -- [1]
    -- >>> maybeToList Nothing
    -- []
  maybeToList :: Maybe a -> [a]
  maybeToList Nothing = []
  maybeToList (Just val) = [val]
  -- >>> catMaybes [Just 1, Nothing, Just 2]
  -- [1, 2]
  -- >>> catMaybes [Nothing, Nothing, Nothing]
  -- []
  catMaybes :: [Maybe a] -> [a]
  catMaybes [] = []
  catMaybes (Nothing:xs) = catMaybes xs
  catMaybes (x:xs) = fromJust x : catMaybes (tail xs)
  -- >>> flipMaybe [Just 1, Just 2, Just 3]
  -- Just [1, 2, 3]
  -- >>> flipMaybe [Just 1, Nothing, Just 3]
  -- Nothing
  flipMaybe :: Eq a => [Maybe a] -> Maybe [a]
  flipMaybe xs =
    if Nothing `elem` xs
    then Nothing
    else Just $ map fromJust xs

  lefts' :: [Either a b] -> [a]
  lefts' = foldr f [] where
    f e acc = case e of
      Left x  -> x : acc
      Right x -> acc

  rights' :: [Either a b] -> [b]
  rights' = foldr f [] where
    f e acc = case e of
      Left x  -> acc
      Right x -> x : acc

  partitionEithers' :: [Either a b] -> ([a], [b])
  partitionEithers' = let
    f e (a', b') = case e of
        Left  x -> (x : a', b')
        Right y -> (a', y : b')
    in foldr f ([],[])

  eitherMaybe' :: (b -> c) -> Either a b -> Maybe c
  eitherMaybe' f (Left a') = Nothing
  eitherMaybe' f (Right b') = Just $ f b'

  --5. This is a general catamorphism for Either values.
  either' :: (a -> c) -> (b -> c) -> Either a b -> c
  either' f _ (Left x) = f x
  either' _ f (Right y) = f y

{--
Write the function myIterate using direct recursion.
Compare the behavior with the built-in iterate to gauge correctness.
Do not look at the source or any examples of iterate so that you are forced to do this yourself.
--}

  myIterate :: (a -> a) -> a -> [a]
  myIterate f a' = a' : myIterate f (f a')


  myUnfoldr :: (b -> Maybe (a, b)) -> b -> [a]
  myUnfoldr f b' = undefined 
