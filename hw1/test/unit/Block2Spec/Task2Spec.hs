module Block2Spec.Task2Spec
  ( spec
  )
where

import Block2.Task2 ( joinWith, splitOn)
import Data.List.NonEmpty
import Test.Hspec

spec :: Spec
spec = do
  describe "splitOn and joinWith" $ do
    it "split given list by the element" $ do
      splitOn '/' "path/to/file" `shouldBe` "path" :| ["to", "file"]
      splitOn '/' "///" `shouldBe` "" :| ["", "", ""]
      splitOn (1 :: Integer) ([1, 2, 3, 1, 2, 3, 1, 2, 3] :: [Integer])
        `shouldBe` 
        [] :| [[2, 3], [2, 3], [2, 3]]
      splitOn (1 :: Integer) ([1, 1, 1, 2, 3, 1, 2, 2, 1, 1] :: [Integer])
        `shouldBe` 
        [] :| [[], [], [2, 3], [2, 2], [], []]
    it "joinWith element list of lists" $ do 
      joinWith '/' (splitOn '/' "path/to/file") `shouldBe` "path/to/file"
      joinWith '/' (splitOn '/' "///") `shouldBe` "///"