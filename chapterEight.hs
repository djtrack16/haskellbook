module ChapterEight where

  cattyConny :: String -> String -> String
  cattyConny x y = x ++ " mrow " ++ y

-- fill in the types
  flippy :: String -> String -> String
  flippy = flip cattyConny

  appedCatty :: String -> String
  appedCatty = cattyConny "woops"

  frappe :: String -> String
  frappe = flippy "haha"

  dividedBy :: Integral a => a -> a -> (a, a)
  dividedBy num denom = go num denom 0
    where go n d count
           | n < d      = (count, n)
           | otherwise  = go (n - d) d (count + 1)

  recurSum :: (Ord a, Num a) => a -> a
  recurSum n = if n <= 1 then n else n + recurSum(n - 1)

  recurMult :: (Integral a) => a -> a -> a
  recurMult x y = if y == 0 then 0 else x + recurMult x (y-1)

  mc91 :: (Integral a) => a -> a
  mc91 n = if n > 100 then n - 10 else mc91 $ mc91 (n + 11)