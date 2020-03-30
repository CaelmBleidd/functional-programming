{-# LANGUAGE LambdaCase #-}

module Block6.Task2 
  ( element
  , eof
  , ok
  , satisfy
  , stream
  ) 
where

import Block6.Task1 (Parser(..))
import Data.List (isPrefixOf)

-- | Success on all input data, don't 'eat' input
ok :: Monoid a => Parser s a
ok = Parser $ \s -> Just (mempty, s)

-- | Returns Just (mempty, s) if reached the end 
-- | of the data stream, Nothing otherwise
eof :: Monoid a => Parser s a
eof = Parser $ \s -> case s of
  [] -> Just (mempty, s)
  _  -> Nothing

-- | Returns Just (x, rest) if x satisfies
-- | to given predicat, Nothing otherwise
satisfy :: (a -> Bool) -> Parser a a
satisfy p = Parser $ \case
  []     -> Nothing
  (x:xs) -> if p x then Just (x, xs) else Nothing

-- | Returns Just (x, rest) if x is equal to 
-- | given element
element :: Eq a => a -> Parser a a
element x = satisfy (== x)

-- | Same as element, but takes list of elements
stream :: Eq a => [a] -> Parser a [a]
stream line =
  Parser $ \s ->
    if line `isPrefixOf` s
      then Just (line, drop (length line) s)
      else Nothing
