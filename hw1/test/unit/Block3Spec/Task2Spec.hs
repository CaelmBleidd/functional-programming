module Block3Spec.Task2Spec
  ( spec
  )
where

import Block3.Task2
import Test.Hspec

calc :: ThisOrThat Integer Integer -> ThisOrThat Integer Integer
calc = id

calc2 :: ThisOrThat Integer String -> ThisOrThat Integer String
calc2 = id

spec :: Spec
spec = do
  let root = "root" :: String
  let server = "server" :: String
  let my = "my" :: String

  describe "Semigroups" $ do
    it "Semigroup for NonEmpty" $ do
      ((1 :: Integer) :| [1, 2, 3]) <> ((1 :: Integer) :| [1, 2, 3]) 
        `shouldBe` (1 :: Integer) :| [1, 2, 3, 1, 1, 2, 3]
      (1 :: Integer) :| [] <> (2 :: Integer) :| [] <> (3 :: Integer) :| [] 
        `shouldBe` (1 :: Integer) :| [2, 3]
      ((1 :: Integer) :| [] <> (2 :: Integer) :| []) <> (3 :: Integer) :| [] 
        `shouldBe` (1 :: Integer) :| [] <> (2 :| [] <> 3 :| [])
    it "Semigroup for ThisOrThat" $ do
      calc2 (This (1 :: Integer)) <> That "text" 
        `shouldBe` Both (1 :: Integer) "text"
      calc2 (This (1 :: Integer) <> This (2 :: Integer))
        `shouldBe` This (1 :: Integer)
      calc2 (This (1 :: Integer) <> That "Text" <> This (2 :: Integer))
        `shouldBe` Both (1 :: Integer) "Text"
      calc ((This (1 :: Integer) <> This (2 :: Integer)) <> That (3 :: Integer)) 
        `shouldBe` Both (1 :: Integer) (3 :: Integer)
      calc (This (1 :: Integer) <> (This (2 :: Integer) <> That (3 :: Integer))) 
        `shouldBe` Both (1 :: Integer) (3 :: Integer)
      calc (This (1 :: Integer) <> This (2 :: Integer) <> That (3 :: Integer))
        `shouldBe` Both (1 :: Integer) (3 :: Integer)
    it "Semigroup for MyString" $ do
      ((Name root <> Name server) :: MyString String) `shouldBe` (Name ("root.server" :: String) :: MyString String) 
      ((Name "" <> Name server) :: MyString String) `shouldBe` ((Name server) :: MyString String)
      ((Name root <> Name "") :: MyString String) `shouldBe` Name root
      (((Name root <> Name server) <> Name my) :: MyString String) `shouldBe` Name "root.server.my"
      ((Name root <> (Name server <> Name my)) :: MyString String) `shouldBe` Name "root.server.my"

      