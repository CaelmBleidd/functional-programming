module Block1Spec.Task1Spec
  ( spec
  ) where

import Block1.Task1
import Test.Hspec

spec :: Spec
spec = do
  describe "Block1.Task1" $ do
    it "returns weekday after given number of days" $ do 
        afterDays Monday 7 `shouldBe` Monday
        afterDays Monday 1 `shouldBe` Tuesday
        afterDays Monday 4 `shouldBe` Friday
        afterDays Monday 13 `shouldBe` Sunday
    it "returns number of day between given weekday and friday" $ do 
        daysToParty Saturday `shouldBe` 6
    it "returns if the given weekday is weekend or not" $ do
        isWeekend Monday `shouldBe` False
        isWeekend Sunday `shouldBe` True
    it "returns next day for given" $ do
        nextDay Monday `shouldBe` Tuesday
        nextDay Sunday `shouldBe` Monday