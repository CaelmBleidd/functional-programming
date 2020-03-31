module Block6Spec.Task4Spec
  ( spec
  )
where

import Block6.Task1
import Block6.Task4
import Test.Hspec

spec :: Spec
spec = do
  describe "Block6.Task4" $ do
    it "listlistParser" $ do
       runParser listlistParser "2, 1,+10  , 3,5,-7, 2"
         `shouldBe` Just ([[1, 10] :: [Int], [5, -7, 2]], "")
       runParser listlistParser "  0    , 1       , 2 " `shouldBe` Just ([[], [2] :: [Int]], "")
       runParser listlistParser "-2, 1,+10  , 3,5,-7, 2" `shouldBe` Nothing
       runParser listlistParser "0" `shouldBe` Just ([[]], "")
       runParser listlistParser "" `shouldBe` Nothing
    it "empty input" $ do
      runParser listlistParser "" `shouldBe` Nothing
    it "ignores spaces" $ do
      runParser listlistParser "  2    ,1, +10,    3, 5,       -7,    2    " `shouldBe`
        Just ([[1, 10], [5, -7, 2]], "")
    it "fails on spaces between digits" $ do
      runParser listlistParser "1, 1 0" `shouldBe` Nothing
    it "fails on spaces between digits 2" $ do
      runParser listlistParser "1 1, 0" `shouldBe` Nothing
    it "fails on spaces between digits 3" $ do
      runParser listlistParser "1 1 0" `shouldBe` Nothing
