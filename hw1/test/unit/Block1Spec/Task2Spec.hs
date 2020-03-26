module Block1Spec.Task2Spec
  ( spec
  )
where

import Block1.Task2
import Prelude hiding (div, mod, toInteger)
import Test.Hspec

spec :: Spec
spec = do
  describe "Task2.Nat" $ do
    it "returns a + b" $ do
      (fromInteger (3) :: Nat)
        +          (fromInteger (5) :: Nat)
        `shouldBe` (fromInteger (8) :: Nat)
      (fromInteger (6) :: Nat)
        +          (fromInteger (0) :: Nat)
        `shouldBe` (fromInteger (6) :: Nat)
      (fromInteger (6) :: Nat)
        +          (fromInteger (1) :: Nat)
        `shouldBe` (fromInteger (7) :: Nat)
    it "returns a * b" $ do
      (fromInteger (3) :: Nat)
        *          (fromInteger (5) :: Nat)
        `shouldBe` (fromInteger (15) :: Nat)
      (fromInteger (6) :: Nat)
        *          (fromInteger (0) :: Nat)
        `shouldBe` (fromInteger (0) :: Nat)
      (fromInteger (6) :: Nat)
        *          (fromInteger (1) :: Nat)
        `shouldBe` (fromInteger (6) :: Nat)
    it "returns a - b" $ do
      (fromInteger (4) :: Nat)
        -          (fromInteger (3) :: Nat)
        `shouldBe` (fromInteger (1) :: Nat)
      (fromInteger (4) :: Nat)
        -          (fromInteger (4) :: Nat)
        `shouldBe` (fromInteger (0) :: Nat)
    it "returns Nat from Integer" $ do
      (fromInteger (3) :: Nat) `shouldBe` (S $ S $ S Z)
      (fromInteger (0) :: Nat) `shouldBe` Z
    it "returns Integer from Nat" $ do
      toInteger ((S $ S $ S Z) :: Nat) `shouldBe` 3
      toInteger (Z :: Nat) `shouldBe` 0
    it "returns parity of number" $ do
      isEven (fromInteger (3) :: Nat) `shouldBe` False
      isEven (fromInteger (2) :: Nat) `shouldBe` True
    it "returns if values equals or not" $ do
      (fromInteger (4) :: Nat) == (fromInteger (4) :: Nat) `shouldBe` True
      (fromInteger (4) :: Nat) == (fromInteger (3) :: Nat) `shouldBe` False
    it "returns if the left value smaller of equals that right value" $ do
      (fromInteger (4) :: Nat) <= (fromInteger (4) :: Nat) `shouldBe` True
      (fromInteger (4) :: Nat) <= (fromInteger (3) :: Nat) `shouldBe` False
      (fromInteger (4) :: Nat) <= (fromInteger (5) :: Nat) `shouldBe` True
    it "returns a div b" $ do
      div (fromInteger (5) :: Nat) (fromInteger (4) :: Nat)
        `shouldBe` (fromInteger (1) :: Nat)
      div (fromInteger (15) :: Nat) (fromInteger (3) :: Nat)
        `shouldBe` (fromInteger (5) :: Nat)
    it "returns a mod b" $ do
      mod (fromInteger (6) :: Nat) (fromInteger (4) :: Nat)
        `shouldBe` (fromInteger (2) :: Nat)
      mod (fromInteger (15) :: Nat) (fromInteger (3) :: Nat)
        `shouldBe` (fromInteger (0) :: Nat)
