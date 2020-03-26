module Block2Spec.Task1Spec
  ( spec
  )
where

import Block1.Task3
import Block2.Task1()
import Data.Foldable
import Data.List (sort)
import Test.Hspec
import Test.QuickCheck

spec :: Spec
spec = do
  describe "Block2.Task1 Tree" $ do
    it "from list to list" $ do
      property $ \l -> Data.Foldable.toList (fromList (l :: [Integer])) == Data.List.sort l