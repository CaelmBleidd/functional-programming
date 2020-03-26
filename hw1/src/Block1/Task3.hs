{-# LANGUAGE InstanceSigs #-}
module Block1.Task3
  ( isEmpty
  , insertElem
  , findElem
  , fromList
  , removeElem
  , size
  , toList
  , BinarySearchTree(..)
  )
where

import Data.List.NonEmpty hiding ( fromList, toList )

-- | Implementation of BST
-- | Leaf does not contains data
-- | Branch contains data and two subtrees:
-- | left child has value less then the value in current branch
-- | right child has value greater then the value in current branch
-- | branch contains NonEmpty List of elements
data BinarySearchTree a = Branch (NonEmpty a) (BinarySearchTree a) (BinarySearchTree a) 
                        | Leaf deriving (Show)

-- | Two trees are equal then and only then when 
-- | all their child and values equal
instance Ord a => Eq (BinarySearchTree a) where
  (==) :: BinarySearchTree a -> BinarySearchTree a -> Bool
  (==) Leaf Leaf = True
  (==) Leaf _    = False
  (==) _    Leaf = False
  (==) (Branch leftValue left right) (Branch rightValue left2 right2) =
    leftValue == rightValue && left == left2 && right == right2

-- | Returns True if given tree is empty, False otherwise
isEmpty :: BinarySearchTree a -> Bool
isEmpty Leaf = True
isEmpty _    = False

-- | Returns amount of node in given tree
size :: BinarySearchTree a -> Int
size Leaf                  = 0
size (Branch _ left right) = size left + size right + 1

-- | Finds element in given tree and returns Just (NonEmpty a) 
-- | if value presents and Nothing otherwise
findElem :: Ord a => a -> BinarySearchTree a -> Maybe (NonEmpty a)
findElem _ Leaf = Nothing
findElem element (Branch value@(x :| _) left right)
  | x == element = Just value
  | x < element  = findElem element right
  | otherwise    = findElem element left

-- | Insert element in given tree and returns modified tree
insertElem :: Ord a => a -> BinarySearchTree a -> BinarySearchTree a
insertElem element Leaf = Branch (element :| []) Leaf Leaf
insertElem element (Branch value@(x :| _) left right)
  | x == element = Branch (x <| value) left right
  | x < element  = Branch value left (insertElem element right)
  | otherwise    = Branch value (insertElem element left) right

-- | Creates a tree from given list
fromList :: Ord a => [a] -> BinarySearchTree a
fromList = foldr insertElem Leaf

-- | Removes node contains specified element and returns 
-- | modified tree
removeElem :: Ord a => a -> BinarySearchTree a -> BinarySearchTree a
removeElem _ Leaf = Leaf
removeElem element node@(Branch value@(x :| _) left right)
  | element == x = removeElem' node
  | element > x  = Branch value left (removeElem element right)
  | otherwise    = Branch value (removeElem element left) right

removeElem' :: Ord a => BinarySearchTree a -> BinarySearchTree a
removeElem' Leaf                  = Leaf
removeElem' (Branch _ Leaf Leaf ) = Leaf
removeElem' (Branch _ Leaf right) = right
removeElem' (Branch _ left Leaf ) = left
removeElem' (Branch _ left right) = Branch smallestValue left newRight
 where
  smallestValue@(x :| _) = getTheSmallestValue right
  newRight               = removeElem x right

getTheSmallestValue :: BinarySearchTree a -> NonEmpty a
getTheSmallestValue Leaf                 = error "Leaf hasn't value"
getTheSmallestValue (Branch x Leaf Leaf) = x
getTheSmallestValue (Branch _ left _   ) = getTheSmallestValue left

-- | Creates list by given BST
-- | List satisfies the equality: toList . fromList = sorted 
toList :: BinarySearchTree a -> [a]
toList Leaf = []
toList (Branch (x :| xs) left right) = (toList left) ++ (x:xs) ++ (toList right)

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
