{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Avoid lambda" #-}
module ChapterTwentyFive where

  import Control.Applicative

  newtype Compose f g a = 
    Compose { getCompose :: f (g a) }
    deriving (Eq, Show)

  instance (Functor f, Functor g) => Functor (Compose f g) where

    fmap :: (Functor f, Functor g) => (a -> b) -> Compose f g a -> Compose f g b
    fmap f (Compose fga) = Compose $ (fmap . fmap) f fga 

  instance (Applicative f, Applicative g) => Applicative (Compose f g) where
    
    pure :: (Applicative f, Applicative g) => a -> Compose f g a
    pure a = Compose $ pure $ pure a

    --liftA2 :: (Applicative f, Applicative g) => (a -> b -> c) -> Compose f g a -> Compose f g b -> Compose f g c
    --liftA2 f (Compose fga) (Compose fgb) = Compose ((fmap . fmap) f fga)

    (<*>) :: (Applicative f, Applicative g) => Compose f g (a -> b) -> Compose f g a -> Compose f g b
    (<*>) (Compose fgf) (Compose fga) = Compose ( fmap (<*>) fgf <*> fga )




-- Compose Foldable
-- Write the Foldable instance for Compose.
-- The foldMap = undefined bit is a hint to make it easier and look more like what you’ve seen already.
  instance (Foldable f, Foldable g) => Foldable (Compose f g) where

    foldMap :: (Foldable f, Foldable g, Monoid m) => (a -> m) -> Compose f g a -> m
    foldMap f (Compose fga) = fmap (foldMap . f . f) fga

--Write the Traversable instance for Compose.
  instance (Traversable f, Traversable g) => Traversable (Compose f g) where

    traverse :: (Traversable f, Traversable g, Applicative f1) => (a -> f1 b) -> Compose f g a -> f1 (Compose f g b)
    traverse f (Compose fga) = fmap . traverse f . traverse f $ fga

