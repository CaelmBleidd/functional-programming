{-# LANGUAGE LambdaCase #-}
module Block6.Task4 where

import Block6.Task1
import Block6.Task2 (element, eof)
import Block6.Task3 (numParser, spaces)
import Control.Applicative ((<|>))
import Control.Monad ((>=>))

-- | Takes description of list of lists and build [[a]]
-- | Description in format:
-- | "numberOfElement /elements with comma/, numberOfElements ..."
-- | For example, for call `runParser listlistParser "2, 1, 2, 3, 1, 2, 3"`
-- | output is Just ([[1, 2], [1, 2, 3]], "")
-- | For empty input string returns Nothing
listlistParser :: Parser Char [[Int]]
listlistParser = Parser $
  runParser numParser >=> \(amount, rest) ->
    runParser ((listParser amount) ++++ (spaces *> (listlistParser' <|> eof))) rest

listlistParser' :: Parser Char [[Int]] 
listlistParser' = Parser $ 
  runParser (spaces *> (element ',') *> numParser) >=> \(amount, rest) ->
    runParser ((listParser amount) ++++ (spaces *> (listlistParser' <|> eof))) rest 

listParser :: Int -> Parser Char [Int]
listParser amount = Parser $ \s ->
  if amount < 0
    then Nothing
    else if amount == 0
      then Just ([], s)
      else runParser
        ((spaces *> (element ',') *> numParser) ++++ (listParser (amount - 1))) s

(++++) :: Parser s a -> Parser s [a] -> Parser s [a]
(++++) first second = Parser $
  runParser first >=> \(result, rest) -> runParser second rest >>= \(secondElem, secondRest) ->
    Just ([result] ++ secondElem, secondRest)
