module Block3Spec.Task1Spec
  ( spec
  )
where

import Block3.Task1
import Data.Monoid
import Test.Hspec

spec :: Spec
spec = do
  describe "Block3.Task1 maybeConcat, eitherConcat" $ do
    it "concat for [Maybe [a]]" $ do
      maybeConcat [Just [1, 2, 3], Nothing, Just [4, 5]]
        `shouldBe` ([1, 2, 3, 4, 5] :: [Integer])
      maybeConcat [Nothing, Nothing, Nothing] `shouldBe` ([] :: [Integer])
    it "concat all Left and Right separately" $ do
      eitherConcat
          [ Left (Sum (3 :: Integer))
          , Right ([1, 2, 3] :: [Integer])
          , Left (Sum (5 :: Integer))
          , Right ([4, 5] :: [Integer])
          ]
        `shouldBe` (Sum { getSum = 8 }, ([1, 2, 3, 4, 5] :: [Integer]))
