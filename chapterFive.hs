module ChapterFive where

  data Mood = Blah | Woot deriving Show

  changeMood :: Mood -> Mood
  changeMood Woot = Blah
  changeMood Blah = Woot

  main :: IO ()
  main = do
    putStrLn ""

  isPalindrome :: (Eq a) => [a] -> Bool
  isPalindrome x = reverse x == x

  myAbs :: Integer -> Integer
  myAbs n = if n < 0 then (-1)*n else n

  f :: (a, b) -> (c, d) -> ((b, d), (a, c))
  f x y = ( (snd x, snd y), (fst x, fst y) )

  x = (+)
  zyx xs = x w 1
    where w = length xs

  imp :: a -> a -> a
  imp x z = x

  imp2 :: a -> b -> b
  imp2 x y = y

  functionH :: [a] -> a
  functionH (x:_) = x

  functionC :: (Ord a) => a -> a -> Bool
  functionC x y = x > y

  functionS :: (a, b) -> b
  functionS (x, y) = y


  i :: a -> a
  i x = x


  c :: a -> b -> a
  c x y = x

  c' :: a -> b -> b
  c' x y = y

  r :: [a] -> [a]
  r (_:xs) = xs

  co :: (b -> c) -> (a -> b) -> (a -> c)
  co f g = f . g


  a :: (a -> c) -> a -> a
  a f x = x

  a' :: (a -> b) -> a -> b
  a' f = f

