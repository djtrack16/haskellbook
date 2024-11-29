{-# LANGUAGE UndecidableInstances #-}
module TransformerExercises where
  import Control.Monad.Trans.Class (MonadTrans (..))
  import Control.Monad.IO.Class (MonadIO (..))
  
  {--
  1. Write the Functor instance for EitherT:
    instance Functor m => Functor (EitherT e m) where
      fmap = undefined
  2. Write the Applicative instance for EitherT:
    instance Applicative m => Applicative (EitherT e m) where pure = undefined
    f <*> a = undefined
  3. Write the Monad instance for EitherT:
  instance Monad m => Monad (EitherT e m) where return = pure
  v >>= f = undefined
  4. Write the swapEitherT helper function for EitherT.
  -- transformer version of swapEither.
  swapEitherT :: (Functor m) => EitherT e m a -> EitherT a m e swapEitherT = undefined
  Hint: write swapEither first, then swapEitherT in terms of the
  former.
  5. Write the transformer variant of the either catamorphism. eitherT :: Monad m =>
  (a -> m c)
  -> (b -> m c)
  -> EitherT a m b -> m c
  eitherT = undefined
--}

  newtype MaybeT m a =
    MaybeT { runMaybeT :: m (Maybe a) }

  newtype EitherT e m a =
    EitherT { runEitherT :: m (Either e a) }

  instance Functor m => Functor (EitherT e m) where

    fmap :: Functor m => (a -> b) -> EitherT e m a -> EitherT e m b
    fmap f (EitherT ema) = EitherT $ (fmap . fmap) f ema

  instance Applicative m => Applicative (EitherT e m) where

    pure :: Applicative m => a -> EitherT e m a
    pure a = EitherT $ pure $ pure a

    (<*>) :: Applicative m => EitherT e m (a -> b) -> EitherT e m a -> EitherT e m b
    (<*>) (EitherT emf) (EitherT ema) = EitherT $ fmap (<*>) emf <*> ema

  instance Monad m => Monad (EitherT e m) where
    return = pure

    (>>=) :: Monad m => EitherT e m a -> (a -> EitherT e m b) -> EitherT e m b
    (>>=) (EitherT ema) f = EitherT $ do
                            either <- ema
                            case either of
                              Left e  -> return $ Left e
                              Right a -> runEitherT (f a)

  swapEitherT :: (Functor m, Monad m) => EitherT e m a -> EitherT a m e
  swapEitherT (EitherT ema) = EitherT $ do
                              fmap swapEither ema

  swapEither :: Either a b -> Either b a
  swapEither (Left a) = Right a
  swapEither (Right b) = Left b

  eitherT :: Monad m => (a -> m c) -> (b -> m c) -> EitherT a m b -> m c
  eitherT fa fb (EitherT amb) = do
                                either <- amb
                                case either of
                                  Left a -> fa a
                                  Right b -> fb b

  newtype ReaderT r m a =
    ReaderT { runReaderT :: r -> m a }

  instance Functor m => Functor (ReaderT r m) where

    fmap :: (a -> b) -> ReaderT r m a -> ReaderT r m b
    fmap f (ReaderT rma) = ReaderT $ (fmap . fmap) f rma

  instance Applicative m => Applicative (ReaderT r m) where

    pure :: Applicative m => a -> ReaderT r m a
    pure a = ReaderT $ pure $ pure a

    (<*>) :: Applicative m => ReaderT r m (a -> b) -> ReaderT r m a -> ReaderT r m b
    (<*>) (ReaderT rmf) (ReaderT rma) = ReaderT $ fmap (<*>) rmf <*> rma

  instance Monad m => Monad (ReaderT r m) where

    (>>=) :: Monad m => ReaderT r m a -> (a -> ReaderT r m b) -> ReaderT r m b
    (>>=) (ReaderT rma) f = ReaderT $ \r -> do
                            a <- rma r
                            runReaderT (f a) r

{--
  newtype StateT s m a =
  StateT { runStateT :: s -> m (a,s) }
  StateT Exercises
  If you’re familiar with the distinction, you’ll be implementing the strict variant of StateT here.
  To make the strict variant, you don’t have to do anything special. Just write the most obvious thing
  that could work. The lazy (lazier, anyway) variant is the one that involves adding a bit extra.
  We’ll explain the difference in the chapter on nonstrictness.
  1. You’ll have to do the Functor and Applicative instances first, because there aren’t Functor and Applicative instances ready to goforthetypeMonad m => s -> m (a, s).
  instance (Functor m) => Functor (StateT s m) where
  fmap f m = undefined
  2. As with Functor, you can’t cheat and re-use an underlying Ap- plicative instance, so you’ll have to do the work with the s -> m (a, s) type yourself.
  CHAPTER26. MONADTRANSFORMERS 935
  instance (Monad m) => Applicative (StateT s m) where pure = undefined
  (<*>) = undefined
  Also note that the constraint on 𝑚 is not Applicative as you expect, but rather Monad. This is because you can’t express the order- dependent computation you’d expect the StateT Applicative to have without having a Monad for 𝑚. To learn more, see this Stack Overflow question1 about this issue. Also see this Github issue2 on the NICTA Course Github repository. Beware! The NICTA course issue gives away the answer. In essence, the issue is that without Monad, you’re just feeding the initial state to each computation in StateT rather than threading it through as you go. This is a general pattern contrasting Applicative and Monad and is worth contemplating.
  3. The Monad instance should look fairly similar to the Monad instance you wrote for ReaderT.
  instance (Monad m) => Monad (StateT s m) where
  return = pure
  sma >>= f = undefined
--}

  newtype StateT s m a =
    StateT { runStateT :: s -> m (a,s) }

  instance (Functor m, Monad m) => Functor (StateT s m) where

    fmap :: Functor m => (a -> b) -> StateT s m a -> StateT s m b
    fmap f (StateT smas) = StateT $ \s -> do
      (a,s) <- smas s
      return (f a, s)

  instance (Monad m) => Applicative (StateT s m) where

    pure :: Monad m => a -> StateT s m a
    pure a = undefined--StateT $ pure $ pure a

    (<*>) :: Monad m => StateT s m (a -> b) -> StateT s m a -> StateT s m b
    (<*>) (StateT smf) (StateT sma) = StateT $ \s -> do
      (f,s) <- smf s
      (a,s) <- sma s
      return (f a, s)

  instance (Monad m) => Monad (StateT s m) where
    return = pure

    (>>=) :: Monad m => StateT s m a -> (a -> StateT s m b) -> StateT s m b
    (>>=) (StateT sma) f = StateT $ \s -> do
                            (a,_) <- sma s
                            runStateT (f a) s


  instance MonadTrans (EitherT e) where

    lift :: Monad m => m a -> EitherT e m a
    lift ma = EitherT $ fmap Right ma

  instance MonadTrans MaybeT where

    lift :: Monad m => m a -> MaybeT m a
    lift ma = MaybeT $ fmap Just ma

  instance MonadTrans (StateT s) where

    lift :: Monad m => m a -> StateT s m a
    lift ma = StateT $ \s -> fmap (,s) ma

  instance MonadTrans (ReaderT r) where

    lift :: Monad m => m a -> ReaderT r m a
    lift ma = ReaderT $ const ma

  instance (MonadIO m, Monad (MaybeT m)) => MonadIO (MaybeT m) where

    liftIO :: (MonadIO m, Monad (MaybeT m)) => IO a -> MaybeT m a
    liftIO = lift . liftIO

  instance (MonadIO m) => MonadIO (ReaderT r m) where

    liftIO :: MonadIO m => IO a -> ReaderT r m a
    liftIO = lift . liftIO
    
  instance (MonadIO m) => MonadIO (StateT s m) where
    liftIO :: MonadIO m => IO a -> StateT s m a
    liftIO = lift . liftIO
