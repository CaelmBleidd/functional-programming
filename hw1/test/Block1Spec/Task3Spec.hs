module Block1Spec.Task3Spec
  ( spec
  )
where

import Block1.Task3
import Data.List.NonEmpty hiding ( fromList, toList )
import Test.Hspec

spec :: Spec
spec = do
  let nonEmptyValue  = ((1 :: Integer) :| [])
  let nonEmptyValue1 = ((2 :: Integer) :| [])
  let nonEmptyValue2 = ((3 :: Integer) :| [])
  let nonEmptyValue3 = ((4 :: Integer) :| [])


  describe "Block1.Task3 Tree" $ do
    it "returns if tree is empty or not" $ do
      isEmpty Leaf `shouldBe` True
      isEmpty (Branch nonEmptyValue Leaf Leaf) `shouldBe` False
    it "returns size of the tree" $ do
      size Leaf `shouldBe` 0
      size
          (Branch nonEmptyValue
                  (Branch nonEmptyValue Leaf Leaf)
                  (Branch nonEmptyValue Leaf Leaf)
          )
        `shouldBe` 3
    it "insert element into the tree" $ do
      insertElem (1 :: Integer) Leaf `shouldBe` (Branch nonEmptyValue Leaf Leaf)
      insertElem (1 :: Integer) (Branch nonEmptyValue Leaf Leaf)
        `shouldBe` (Branch (1 :| [1]) Leaf Leaf)
      insertElem (2 :: Integer) (Branch nonEmptyValue2 Leaf Leaf)
        `shouldBe` (Branch nonEmptyValue2 (Branch nonEmptyValue1 Leaf Leaf) Leaf
                   )
    it "find element into the tree" $ do
      findElem (1 :: Integer) Leaf `shouldBe` Nothing
      findElem (1 :: Integer) (Branch nonEmptyValue Leaf Leaf)
        `shouldBe` Just nonEmptyValue
      findElem (2 :: Integer)
               (Branch nonEmptyValue2 (Branch nonEmptyValue1 Leaf Leaf) Leaf)
        `shouldBe` Just nonEmptyValue1
    it "create a tree from given list" $ do
      fromList ([4, 1, 2, 3] :: [Integer]) `shouldBe` Branch
        nonEmptyValue2
        (Branch nonEmptyValue1 (Branch nonEmptyValue Leaf Leaf) Leaf)
        (Branch nonEmptyValue3 Leaf Leaf)
    it "create a list from given tree (list will be sorted)" $ do 
      toList (fromList ([4, 1, 2, 3] :: [Integer])) `shouldBe` [1, 2, 3, 4]
    it "remove element from the tree if it presents" $ do
      removeElem (1 :: Integer) Leaf `shouldBe` Leaf
      removeElem (1 :: Integer) (Branch nonEmptyValue3 Leaf Leaf)
        `shouldBe` (Branch nonEmptyValue3 Leaf Leaf)
      removeElem
          (1 :: Integer)
          (Branch
            nonEmptyValue2
            (Branch nonEmptyValue1 (Branch nonEmptyValue Leaf Leaf) Leaf)
            (Branch nonEmptyValue3 Leaf Leaf)
          )
        `shouldBe` (Branch nonEmptyValue2
                           (Branch nonEmptyValue1 Leaf Leaf)
                           (Branch nonEmptyValue3 Leaf Leaf)
                   )
      removeElem
          (3 :: Integer)
          (Branch
            nonEmptyValue2
            (Branch nonEmptyValue1 (Branch nonEmptyValue Leaf Leaf) Leaf)
            (Branch nonEmptyValue3 Leaf Leaf)
          )
        `shouldBe` (Branch
                     nonEmptyValue3
                     (Branch nonEmptyValue1
                             (Branch nonEmptyValue Leaf Leaf)
                             Leaf
                     )
                     Leaf
                   )
