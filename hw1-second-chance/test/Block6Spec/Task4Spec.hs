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
