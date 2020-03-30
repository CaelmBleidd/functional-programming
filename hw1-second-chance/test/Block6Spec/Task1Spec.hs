module Block6Spec.Task1Spec
  ( spec
  )
where

import Block6.Task1
import Control.Applicative ((<|>))
import Test.Hspec

spec :: Spec
spec = do
  describe "Block6.Task1" $ do
    it "<$>" $ do
      runParser ((++ " works") <$> (Parser $ \s -> Just (s, ""))) "<$>"
        `shouldBe` Just ("<$> works", "")
    it "<*>" $ do
      runParser ((Parser $ \s -> Just ((++) "<*>", s)) <*> (Parser $ \s -> Just (s, ""))) " works"
        `shouldBe` Just ("<*> works", "")
    it ">>=" $ do
      runParser ((Parser $ \s -> Just ("42", s)) >>= (\a -> Parser $ \s -> Just (a, s))) "4"
        `shouldBe` Just ("42", "4")
    it "<|>" $ do
      runParser ((Parser $ \_ -> Nothing) <|> (Parser $ \s -> Just (s, s))) "42"
        `shouldBe` Just ("42", "42")
      runParser ((Parser $ \s -> Just (s, s)) <|> (Parser $ \_ -> Nothing)) "42"
        `shouldBe` Just ("42", "42")
      runParser ((Parser $ \s -> Just ("", s)) <|> (Parser $ \s -> Just (s, s))) "42"
        `shouldBe` Just ("", "42")
