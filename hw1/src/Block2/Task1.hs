{-# LANGUAGE InstanceSigs #-}
module Block2.Task1
  ()
where

import           Block1.Task3                   ( BinarySearchTree(..) )
import           Data.List.NonEmpty

-- | Implementation of Foldable for BinarySearchTree
instance Foldable BinarySearchTree where
  foldMap :: Monoid m => (a -> m) -> BinarySearchTree a -> m
  foldMap _        Leaf                        = mempty
  foldMap function (Branch (x :| _) Leaf Leaf) = function x
  foldMap function (Branch (x :| _) left right) =
    foldMap function left `mappend` function x `mappend` foldMap function right
  foldr :: (a -> b -> b) -> b -> BinarySearchTree a -> b
  foldr _        z Leaf                        = z
  foldr function z (Branch (x :| _) Leaf Leaf) = function x z
  foldr function z (Branch (x :| _) left right) =
    foldr function (function x (foldr function z right)) left
