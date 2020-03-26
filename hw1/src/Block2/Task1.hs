{-# LANGUAGE InstanceSigs #-}
module Block2.Task1
  ()
where

import           Block1.Task3                   ( BinarySearchTree(..) )

-- | Implementation of Foldable for BinarySearchTree
instance Foldable BinarySearchTree where
  foldMap :: Monoid m => (a -> m) -> BinarySearchTree a -> m
  foldMap _ Leaf = mempty
  foldMap function (Branch x left right) =
    foldMap function left
      <> foldMap function x
      <> foldMap function right
  foldr :: (a -> b -> b) -> b -> BinarySearchTree a -> b
  foldr _ z Leaf = z
  foldr function z (Branch x left right) =
    foldr function (foldr function (foldr function z right) x) left
