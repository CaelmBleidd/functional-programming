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

ok :: Monoid a => Parser s a
ok = Parser $ \s -> Just (mempty, s)

eof :: Monoid a => Parser s a
eof = Parser $ \s -> case s of
  [] -> Just (mempty, s)
  _  -> Nothing

satisfy :: (a -> Bool) -> Parser a a
satisfy p = Parser $ \case
  []     -> Nothing
  (x:xs) -> if p x then Just (x, xs) else Nothing

element :: Eq a => a -> Parser a a
element x = satisfy (== x)

stream :: Eq a => [a] -> Parser a [a]
stream line =
  Parser $ \s ->
    if line `isPrefixOf` s
      then Just (line, drop (length line) s)
      else Nothing
