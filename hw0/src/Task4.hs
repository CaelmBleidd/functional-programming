module Task4
  ( iterateElement
  , factorial
  , fibonacci
  , mapFix
  )
where

import           Data.Function                  ( fix )

-- | Repeat x is an infinite list, with x the value of every element
iterateElement :: a -> [a]
iterateElement x = fix (x :)

-- | Compute n-th fibonacci number
fibonacci :: Integer -> Integer
fibonacci = fix $ \rec n -> if n < 0
  then error "The argument must be non-negative"
  else if n == 0 || n == 1 then 1 else rec (n - 1) + rec (n - 2)

-- | Compute the factorial function n!
factorial :: Integer -> Integer
factorial = fix $ \rec n -> if n < 0
  then error "The argument must be non-negative"
  else if n == 0 || n == 1 then 1 else n * rec (n - 1)

-- | mapFix f xs is the list obtained by applying 
-- | f to each element of xs
mapFix :: (a -> b) -> [a] -> [b]
mapFix = fix $ \rec f sourceList@(x : xs) ->
  if null sourceList then [] else f x : rec f xs
