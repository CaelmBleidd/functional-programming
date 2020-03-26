module Block2.Task2
  ( joinWith
  , splitOn
  )
where

import Data.List.NonEmpty       hiding ( init )
import Prelude                  hiding ( reverse )

-- | Split given list by element into a list of lists
-- | Output list never empty
splitOn :: Eq a => a -> [a] -> NonEmpty [a]
splitOn element list = foldr function ([] :| []) list
 where
  function elemOfList collector@(x :| xs)
    | elemOfList == element = [] <| collector
    | otherwise             = (elemOfList : x) :| xs

-- | Join list of lists into list of elements, separeted by
-- | given element. Satisfies: joinWith elem (splitOn elem list) == list
-- | Input list of lists must be nonempty
joinWith :: Eq a => a -> NonEmpty [a] -> [a]
joinWith element list = init $ foldl function [] list
  where function collector elemOfList = collector ++ elemOfList ++ [element]

