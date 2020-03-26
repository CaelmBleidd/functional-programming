module Block4Spec.Task1Spec
  ( spec
  )
where

import Block4.Task1
import Test.Hspec

spec :: Spec
spec = do
  describe "Block3.Task1 maybeConcat, eitherConcat" $ do
    it "Sum all numbers in the line" $ do 
      stringSum "1    2 3 4 5" `shouldBe` Just (15 :: Int)
      stringSum "-1  2 3 4 5  1" `shouldBe` Just (14 :: Int)
      stringSum "a 1 2 3 4" `shouldBe` Nothing 
      stringSum "- 1 2 3" `shouldBe` Nothing
    