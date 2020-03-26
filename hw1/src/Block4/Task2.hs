{-# LANGUAGE InstanceSigs #-}
module Block4.Task2
  ( Tree(..)
  )
where

data Tree a = Branch (Tree a) (Tree a) | Leaf a deriving (Show)

instance Functor Tree where
  fmap :: (a -> b) -> Tree a -> Tree b
  fmap f (Leaf a           ) = Leaf $ f a
  fmap f (Branch left right) = Branch (fmap f left) (fmap f right)

instance Applicative Tree where
  pure :: a -> Tree a
  pure = Leaf
  (<*>) :: Tree (a -> b) -> Tree a -> Tree b
  (<*>) (Leaf f) (Leaf x           ) = Leaf (f x)
  (<*>) (Leaf f) (Branch left right) = Branch (f <$> left) (f <$> right)
  (<*>) (Branch left right) (Leaf x) =
    Branch (left <*> pure x) (right <*> pure x)
  (<*>) (Branch left right) (Branch left2 right2) =
    Branch (left <*> left2) (right <*> right2)

instance Foldable Tree where
  foldMap :: Monoid m => (a -> m) -> Tree a -> m
  foldMap f (Leaf a           ) = f a
  foldMap f (Branch left right) = foldMap f left <> foldMap f right

instance Traversable Tree where
  traverse :: (Functor f, Applicative f) => (a -> f b) -> Tree a -> f (Tree b)
  traverse f (Leaf x) = Leaf <$> f x
  traverse f (Branch left right) =
    Branch <$> traverse f left <*> traverse f right

