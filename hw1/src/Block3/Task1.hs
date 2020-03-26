module Block3.Task1
  ( eitherConcat
  , maybeConcat
  )
where

import           Data.Maybe                     ( catMaybes )
import           Data.Either

-- | Takes list of Maybe[a] and returns list of 
-- | concatenated Just. For example, 
-- | maybeConcat [Just [1, 2, 3], Nothing, Just [4, 5]] == [1, 2, 3, 4, 5]
maybeConcat :: [Maybe [a]] -> [a]
maybeConcat = concat . catMaybes

-- | Takes list of Either a b and returns pair of 
-- | (leftValues, rightValues), where values if result of 
-- | mconcat on Left and Right values respectively
eitherConcat :: (Monoid a, Monoid b) => [Either a b] -> (a, b)
eitherConcat list = (first, second)
 where
  first  = mconcat $ lefts list
  second = mconcat $ rights list
