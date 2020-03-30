{-# LANGUAGE InstanceSigs #-}

module Block6.Task1
  ( Parser(..)
  )
where

import Control.Applicative
import Control.Monad ((>=>))
import Data.Bifunctor (first)

-- | Parser data structure 
-- | Takes list of elements with type s
-- | Returns Maybe (element of type a, rest of [s])
data Parser s a = Parser { runParser :: [s] -> Maybe (a, [s]) }

instance Functor (Parser s) where
  fmap :: (a -> b) -> Parser s a -> Parser s b
  fmap f (Parser parser) = Parser $ fmap (first f) . parser

instance Applicative (Parser s) where
  pure :: a -> Parser s a
  pure a = Parser $ \s -> Just (a, s)

  (<*>) :: Parser s (a -> b) -> Parser s a -> Parser s b
  parserPf <*> parserPa = Parser $ runParser parserPf >=>
    \(f, t) -> runParser parserPa t >>= \(a, r) -> return (f a, r)

instance Monad (Parser s) where
  (>>=) :: Parser s a -> (a -> Parser s b) -> Parser s b
  (>>=) (Parser runPf) f = Parser $ runPf >=>
    \(a, line) -> runParser (f a) line

instance Alternative (Parser s) where
  empty :: Parser s a
  empty = Parser $ \_ -> Nothing

  (<|>) :: Parser s a -> Parser s a -> Parser s a
  Parser runPf <|> Parser runPa = Parser $
    \s -> case runPf s of
      Nothing -> runPa s
      Just x  -> Just x



