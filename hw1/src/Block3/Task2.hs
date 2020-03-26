{-# LANGUAGE InstanceSigs #-}
module Block3.Task2
  ( Endo(..)
  , MyString(..)
  , NonEmpty(..)
  , ThisOrThat(..)
  )
where

data NonEmpty a = a :| [a] deriving (Show)
data ThisOrThat a b = This a | That b | Both a b deriving (Show)
newtype MyString a = Name String deriving (Show)
newtype Endo a = Endo { getEndo :: a -> a }

instance Ord a => Eq (NonEmpty a) where
  (==) :: NonEmpty a -> NonEmpty a -> Bool
  (==) (x :| xs) (y :| ys) = x == y && xs == ys

instance Semigroup (NonEmpty a) where
  (<>) :: NonEmpty a -> NonEmpty a -> NonEmpty a
  (x :| xs) <> (y :| ys) = x :| (xs ++ y : ys)

instance (Ord a, Ord b) => Eq (ThisOrThat a b) where
  (==) :: ThisOrThat a b -> ThisOrThat a b -> Bool
  (==) (This a  ) (This b  ) = a == b
  (==) (That a  ) (That b  ) = a == b
  (==) (Both a b) (Both c d) = a == c && b == d
  (==) _          _          = False

instance Semigroup (ThisOrThat a b) where
  (<>) :: ThisOrThat a b -> ThisOrThat a b -> ThisOrThat a b
  (This a) <> (That b  ) = Both a b
  (This a) <> (Both _ b) = Both a b
  (That a) <> (This b  ) = Both b a
  (That a) <> (Both c _) = Both c a
  first    <> _          = first

instance Ord a => Eq (MyString a) where
  (==) :: MyString a -> MyString a -> Bool
  (==) (Name a) (Name b) = a == b

instance Semigroup (MyString a) where
  (<>) :: MyString a -> MyString a -> MyString a
  Name "" <> Name b  = Name b
  Name a  <> Name "" = Name a
  Name a  <> Name b  = Name (a ++ "." ++ b)

instance Monoid (MyString a) where
  mempty :: MyString a
  mempty = Name ""

instance Semigroup (Endo a) where
  (<>) :: Endo a -> Endo a -> Endo a
  Endo a <> Endo b = Endo $ a . b

instance Monoid (Endo a) where
  mempty :: Endo a
  mempty = Endo id
