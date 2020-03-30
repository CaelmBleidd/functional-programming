{-# LANGUAGE LambdaCase #-}
module Block6.Task4 where

import Block6.Task1
import Block6.Task2 (element, eof)
import Block6.Task3 (numParser, spaces)
import Control.Applicative ((<|>))
import Control.Monad ((>=>))

listlistParser :: Parser Char [[Int]]
listlistParser = Parser $
  runParser ((spaces *> (element ',') *> numParser) <|> numParser) >=> \(amount, rest) ->
    runParser ((listParser amount) ++++ (spaces *> (listlistParser <|> eof))) rest

listParser :: Int -> Parser Char [Int]
listParser amount = Parser $ \s ->
  if amount < 0
    then Nothing
    else if amount == 0
      then Just ([], s)
      else runParser
        (((spaces *> (element ',') *> numParser) <|> numParser) ++++ (listParser (amount - 1))) s

(++++) :: Parser s a -> Parser s [a] -> Parser s [a]
(++++) first second = Parser $
  runParser first >=> \(result, rest) -> runParser second rest >>= \(secondElem, secondRest) ->
    Just ([result] ++ secondElem, secondRest)
