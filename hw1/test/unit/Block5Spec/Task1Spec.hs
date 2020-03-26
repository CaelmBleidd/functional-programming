module Block5Spec.Task1Spec
  ( spec
  )
where

import Block5.Task1
import Test.Hspec

spec :: Spec
spec = do
  describe "Expressions" $ do
    it "Evaluate plus" $ do
      eval (BinaryOperation Plus (Value 5) (Value 3)) `shouldBe` (Right 8)
      eval (BinaryOperation Plus (Value (-5)) (Value 5)) `shouldBe` (Right 0)
    it ("Evaluate minus") $ do
      eval (BinaryOperation Minus (Value 5) (Value 3)) `shouldBe` (Right 2)
      eval (BinaryOperation Minus (Value (-5)) (Value 5))
        `shouldBe` (Right (-10))
    it ("Evaluate mult") $ do
      eval (BinaryOperation Mult (Value 5) (Value 3)) `shouldBe` (Right 15)
      eval (BinaryOperation Mult (Value (-5)) (Value 5))
        `shouldBe` (Right (-25))
    it ("Evaluate div") $ do
      eval (BinaryOperation Div (Value 5) (Value 3)) `shouldBe` (Right 1)
      eval (BinaryOperation Div (Value (-5)) (Value 5)) `shouldBe` (Right (-1))
      eval (BinaryOperation Div (Value (-5)) (Value 0))
        `shouldBe` (Left DivisionByZero)
    it ("Evaluate pow") $ do
      eval (BinaryOperation Pow (Value 5) (Value 3)) `shouldBe` (Right 125)
      eval (BinaryOperation Pow (Value (-5)) (Value 3))
        `shouldBe` (Right (-125))
      eval (BinaryOperation Pow (Value (-5)) (Value 0)) `shouldBe` (Right 1)
      eval (BinaryOperation Pow (Value 1) (Value (-1)))
        `shouldBe` (Left NegativePow)
    it ("Evaluate combo") $ do
      eval
          (BinaryOperation
            Pow
            (BinaryOperation
              Mult
              (BinaryOperation Div
                               (BinaryOperation Plus (Value 5) (Value 8))
                               (Value 2)
              )
              (BinaryOperation Minus (Value 5) (Value 2))
            )
            (Value 2)
          )
        `shouldBe` (Right (18 * 18))
