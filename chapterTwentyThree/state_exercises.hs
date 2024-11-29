{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use tuple-section" #-}
{-# HLINT ignore "Use evalState" #-}
{-# HLINT ignore "Use execState" #-}
module StateExercises where

  newtype State s a =
    State { runState :: s -> (a, s) }

  instance Functor (State s) where

    fmap :: (a -> b) -> State s a -> State s b
    fmap f (State g) = State (f a,) where (a,s) = g s
    -- reduced from..= State $ \s -> (f a, s) where (a,s) = g s

  instance Applicative (State s) where

    pure :: a -> State s a
    pure a = State (a,)

    (<*>) :: State s (a -> b) -> State s a -> State s b
    (<*>) (State sf) (State sa) = State $ \s -> (f a, s) where
      (f, _) = sf s
      (a,s) = sa s

  instance Monad (State s) where
    return = pure

    (>>=) :: State s a -> (a -> State s b) -> State s b
    (>>=) (State sa) fasb = State $ \s ->
                            let (a, s') = sa s
                            in runState (fasb a) s'

--1. Construct a State where the state is also the value you return.
  get :: State s s
  get = State $ \s -> (s,s)

-- Expected output
-- Prelude> runState get "curryIsAmaze"
-- ("curryIsAmaze","curryIsAmaze")

-- 2. Construct a State where the resulting state is the argument provided and the value is defaulted to unit.
  put :: s -> State s ()
  put s' = State $ const ((), s')

--Prelude> runState (put "blah") "woot"
--((),"blah")

{--  3. Run the State with 𝑠 and get the state that results.
    Prelude> exec (put "wilma") "daphne"
    "wilma"
    Prelude> exec get "scooby papu"
    "scooby papu"
--}

  exec :: State s a -> s -> s
  exec state s = snd $ runState state s

  {--
4. Run the State with 𝑠 and get the value that results.
    Prelude> eval get "bunnicula"
    "bunnicula"
    Prelude> eval get "stake a bunny"
    "stake a bunny"
--}

  eval :: State s a -> s -> a
  eval state s = fst $ runState state s

{--

5. Write a function which applies a function to create a new State.

Should behave like the following:
    Prelude> runState (modify (+1)) 0
    ((),1)
    Prelude> runState (modify (+1) >> modify (+1)) 0
    ((),2)
Note you don’t need to compose them, you can just throw away the result because it returns unit for 𝑎 anyway.

--}

  modify :: (s -> s) -> State s ()
  modify f = State $ \s -> ((),f s)
