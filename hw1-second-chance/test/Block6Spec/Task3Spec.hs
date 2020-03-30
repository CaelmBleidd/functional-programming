module Block6Spec.Task3Spec
  ( spec
  )
where

import Block6.Task1
import Block6.Task3
import Test.Hspec

spec :: Spec
spec = do
  describe "Block6.Task3" $ do
    it "correctBracketSequenceParser" $ do
      runParser correctBracketSequenceParser "" `shouldBe` Just ("", "")
      runParser correctBracketSequenceParser "()" `shouldBe` Just ("()", "")
      runParser correctBracketSequenceParser "()()(((())))" `shouldBe` Just ("()()(((())))", "")
      runParser correctBracketSequenceParser "(((((())))))" `shouldBe` Just ("(((((())))))", "")
      runParser correctBracketSequenceParser "(())(())(())" `shouldBe` Just ("(())(())(())", "")
      runParser correctBracketSequenceParser "(())(())(())(())" `shouldBe` Just ("(())(())(())(())", "")

      runParser correctBracketSequenceParser "(((()))" `shouldBe` Nothing
      runParser correctBracketSequenceParser "((((" `shouldBe` Nothing
      runParser correctBracketSequenceParser ")))" `shouldBe` Nothing
      runParser correctBracketSequenceParser ")" `shouldBe` Nothing
      runParser correctBracketSequenceParser "(" `shouldBe` Nothing
      runParser correctBracketSequenceParser "   (((())))" `shouldBe` Nothing
      runParser correctBracketSequenceParser "(((())))    " `shouldBe` Nothing
      runParser correctBracketSequenceParser "(((())))as" `shouldBe` Nothing

    it "numParser" $ do
      runParser numParser "3" `shouldBe` Just (3, "")
      runParser numParser "   3   " `shouldBe` Just (3, "   ")
      runParser numParser "    +3" `shouldBe` Just (3, "")
      runParser numParser "    -3" `shouldBe` Just (-3, "")
      runParser numParser "    -3asdas" `shouldBe` Just (-3, "asdas")
      runParser numParser "  123, 123, 123, 123" `shouldBe` Just (123, ", 123, 123, 123")
      runParser numParser "++3" `shouldBe` Nothing
      runParser numParser "+-3" `shouldBe` Nothing
      runParser numParser "-+3" `shouldBe` Nothing
      runParser numParser "" `shouldBe` Nothing
