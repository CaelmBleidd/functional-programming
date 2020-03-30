module Block6Spec.Task2Spec
  ( spec
  )
where

import Block6.Task1
import Block6.Task2
import Data.Maybe (isNothing)
import Test.Hspec
import Test.QuickCheck

spec :: Spec
spec = do
  describe "Block6.Task2" $ do
    it "ok" $ do runParser ok "42" `shouldBe` Just ("", "42")
    it "eof" $ do
      runParser eof "" `shouldBe` Just ("", "")
      runParser (eof :: Parser Char [Char]) " " `shouldBe` Nothing
    it "satisfy" $ do
      runParser (satisfy (== ('4'))) "42" `shouldBe` Just ('4', "2")
      runParser (satisfy (== ('4'))) "52" `shouldBe` Nothing
    it "element" $ do
      runParser (element ',') ", " `shouldBe` Just (',', " ")
      runParser (element ',') " " `shouldBe` Nothing
    it "stream" $ do
      runParser (stream "abc") "abcde" `shouldBe` Just ("abc", "de")
      runParser (stream "abs") "bcdea" `shouldBe` Nothing
  describe "Propery based Block6.Task2" $ do
    it "ok" $ do property $ \s -> runParser ok (s :: String) == Just ((), s)
    it "eof" $ do
      property $
        \s ->
          runParser (eof :: Parser Char [Char]) (s :: String) == if null s
            then Just (mempty,"")
            else Nothing
    it "satisfy" $ do
      property $ \s ->
        case s of
          [] -> isNothing (runParser (satisfy (== ' ')) (s :: String))
          (x:xs) -> runParser (satisfy (== x)) (s :: String) == Just (x, xs)
    it "satisfy fail" $ do
      property $ \s ->
         case s of
           [] -> isNothing (runParser (satisfy (/= ' ')) (s :: String))
           (x:_) -> isNothing (runParser (satisfy (/= x)) (s :: String))
    it "element" $ do
      property $ \s ->
        case s of
          [] -> isNothing (runParser (element ' ') (s :: String))
          (x:xs) -> runParser (element x) (s :: String) == Just (x, xs)
    it "stream" $ do
      property $ \s->
        case s of
          [] -> isNothing (runParser (stream " ") (s :: String))
          (x:xs) -> runParser (stream [x]) s == Just ([x], xs)
