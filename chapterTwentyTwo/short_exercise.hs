{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use <$>" #-}
module ShortExercise where

  import Data.Char
  import Control.Applicative
  import Control.Monad

  cap :: [Char] -> [Char]
  cap = map toUpper
  
  rev :: [Char] -> [Char]
  rev = reverse

--  Two simple functions with the same type, taking the same type of
--  input. We could compose them, using (.) or fmap:

  composed :: [Char] -> [Char]
  composed = rev . cap
  
  fmapped :: [Char] -> [Char]
  fmapped = fmap rev cap
{--  
  The output of those two should be identical: one string that is made
  all uppercase and reversed, like this:
  Prelude> composed "Julie"
  "EILUJ"
  Prelude> fmapped "Chris"
  "SIRHC"
  Now we want to return the results of cap and rev both, as a tuple, like this:

  Prelude> tupled "Julie"
  ("JULIE","eiluJ")
  -- or
  Prelude> tupled' "Julie"
  ("eiluJ","JULIE")
  We will want to use an applicative here. The type will look like this:
--}
  tupledA :: [Char] -> ([Char], [Char])
  tupledA = (,) <$> rev <*> cap

  tupledM :: [Char] -> ([Char], [Char])
  tupledM = do
    liftA2 (,) rev cap

  tupledM' :: [Char] -> ([Char], [Char])
  tupledM' = cap >> rev >>= (,)