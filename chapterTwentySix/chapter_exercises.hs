module ChapterExercises where
  import Control.Monad.Trans.Reader
  import Data.Functor.Identity
  import Control.Monad.Trans.Maybe
  import Control.Monad
  import Control.Monad.Trans.State ()
  
  rDec :: Num a => Reader a a
  rDec = reader . (+) $ negate 1

  rShow :: Show a => ReaderT a Identity String
  rShow = ReaderT $ Identity . show
  -- ReaderT $ \r -> Identity $ show r

  rPrintAndInc :: (Num a, Show a) => ReaderT a IO a
  rPrintAndInc = ReaderT $ \r -> do
    putStrLn $ "Hi: " ++ show r
    return (r+1)

  newtype StateT s m a =
    StateT { runStateT :: s -> m (a,s) }

  --newtype ReaderT r m a =
  --  ReaderT { runReaderT :: r -> m a }

{--
sPrintIncAccum first prints the input with a greeting, then puts the incremented input as the new state, and returns the original input as a String.
sPrintIncAccum :: (Num a, Show a) => StateT a IO String sPrintIncAccum = undefined
    Prelude> runStateT sPrintIncAccum 10
    Hi: 10
    ("10",11)
    Prelude> mapM (runStateT sPrintIncAccum) [1..5]
    Hi: 1
    Hi: 2
    Hi: 3
    Hi: 4
    Hi: 5
    [("1",2),("2",3),("3",4),("4",5),("5",6)]
--}

  sPrintIncAccum :: (Num a, Show a) => StateT a IO String
  sPrintIncAccum = StateT $ \s -> do
    putStrLn $ "Hi: " ++ show s
    return (show s, s+1)

{--
  isValid :: String -> Bool
  isValid v = '!' `elem` v

  maybeExcite :: MaybeT IO String
  maybeExcite = do
    v <- getLine
    guard $ isValid v
    return v

  doExcite :: IO ()
  doExcite = do
    putStrLn "say something excite!"
    excite <- maybeExcite
    case excite of
      Nothing -> putStrLn "MOAR EXCITE"
      Just e -> putStrLn ("Good, was very excite: " ++ e)
--}