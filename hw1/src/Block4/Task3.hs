{-# LANGUAGE InstanceSigs #-}
module Block4.Task3
  ( NonEmpty(..)
  )
where

data NonEmpty a = a :| [a] deriving (Show)

instance Functor NonEmpty where
  fmap f (x :| xs) = f x :| map f xs

instance Foldable NonEmpty where
  foldMap :: Monoid b => (a -> b) -> NonEmpty a -> b
  foldMap f (x :| xs) = foldMap f (x : xs)

instance Traversable NonEmpty where
  traverse :: Applicative f => (a -> f b) -> NonEmpty a -> f (NonEmpty b)
  traverse f (x :| xs) = (:|) <$> (f x) <*> traverse f xs

instance Applicative NonEmpty where
  pure :: a -> NonEmpty a
  pure x = x :| []

  (<*>) :: NonEmpty (a -> b) -> NonEmpty a -> NonEmpty b
  (<*>) (f :| fs) (x :| xs) = result $ (f : fs) <*> (x : xs)
   where
    result []       = error "List cannot be empty"
    result (y : ys) = y :| ys


instance Monad NonEmpty where
  return = pure

  (>>=) :: NonEmpty a -> (a -> NonEmpty b) -> NonEmpty b
  (>>=) (x :| []      ) f = f x
  (>>=) (a :| (x : xs)) f = z :| (zs ++ [y] ++ ys)
   where
    (y :| ys) = (x :| xs) >>= f
    (z :| zs) = f a
