{-# LANGUAGE LambdaCase #-}

module Block6.Task3
  ( correctBracketSequenceParser
  , numParser
  , spaces
  )
where

import Block6.Task1
import Block6.Task2 (element, ok, satisfy)
import Control.Applicative (many, some, (<|>))
import Control.Monad ((>=>))
import Data.Char (isDigit)

correctBracketSequenceParser :: Parser Char String
correctBracketSequenceParser = Parser $
  runParser (parensParser 0 "") >=> \((balance, bracketSeq), rest) ->
    if balance /= 0 then Nothing else Just (bracketSeq, rest)

parensParser :: Int -> String -> Parser Char (Int, String)
parensParser balance bracketSeq =
  Parser $ runParser (lParenParser <|> rParenParser) >=>
    \((change, symbol), rest) ->
      case change of
        0 -> Just ((balance, bracketSeq), "")
        _ -> if balance + change < 0
          then Nothing
          else runParser (parensParser (balance + change) (bracketSeq ++ symbol)) rest

lParenParser :: Parser Char (Int, String)
lParenParser = Parser $ \case
  []     -> Just ((0, ""), "")
  (x:xs) -> if x == '(' then Just ((1, "("), xs) else Nothing

rParenParser :: Parser Char (Int, String)
rParenParser = Parser $ \case
  []     -> Just ((0, ""), "")
  (x:xs) -> if x == ')' then Just ((-1, ")"), xs) else Nothing

spaces :: Parser Char [Char]
spaces = many (element ' ')

digits :: Parser Char [Char]
digits = some (satisfy isDigit)

plusSignParser :: Parser Char [Char]
plusSignParser = Parser $ \case
  []     -> Nothing
  (x:xs) -> if x == '+' then Just ([], xs) else Nothing

minusSignParser :: Parser Char [Char]
minusSignParser = Parser $ \case
  []     -> Nothing
  (x:xs) -> if x == '-' then Just ([x], xs) else Nothing

(+++) :: Semigroup a => Parser s a -> Parser s a -> Parser s a
(+++) (Parser runPf) (Parser runPa) = Parser $
  runPf >=> \(x, xs) -> runPa xs >>= \(y, ys) -> Just (x <> y, ys)

numParser :: Parser Char Int
numParser = Parser $
  runParser ((spaces *> (plusSignParser <|> minusSignParser <|> ok)) +++ digits) >=>
    \(match, rest) -> return (read match :: Int, rest)
