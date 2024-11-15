module ChapterSeven where

  add :: Int -> Int -> Int
  add x y = x + y

  addPF :: Int -> Int -> Int
  addPF = (+)

  addOne :: Int -> Int
  addOne x = x + 1
  
  addOnePF :: Int -> Int
  addOnePF = (+1)

  hundredthDigit :: Integral a => a -> a
  hundredthDigit x = d
    where onesD = x `div` 100
          d = onesD `mod` 10

  tensDigitDivMod :: Integral a => a -> a
  tensDigitDivMod x = mod (fst (divMod x 10)) 10

  foldBool :: a -> a -> Bool -> a
  foldBool x y z
    | z  = x
    | otherwise = y
  
  g :: (a -> b) -> (a, c) -> (b, c)
  g f (x, y) = (f x, y)

  main :: IO ()
  main = do
    print (0 :: Int)
    print (add 1 0)
    print (addOne 0)
    print (addOnePF 0)
    print ((addOne . addOne) 0)
    print ((addOnePF . addOne) 0)
    print ((addOne . addOnePF) 0)
    print ((addOnePF . addOnePF) 0)
    print (negate (addOne 0))
    print ((negate . addOne) 0)
    print ((addOne . addOne . addOne . negate . addOne) 0)
