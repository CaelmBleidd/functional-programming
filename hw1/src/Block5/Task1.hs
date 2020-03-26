{-# LANGUAGE InstanceSigs #-}
module Block5.Task1
  ( eval
  , ArithmeticError(..)
  , Expr(..)
  , Op(..)
  )
where

import           Data.Either

data Expr a = Value Int
            | BinaryOperation Op (Expr a) (Expr a)

data Op = Plus
        | Minus
        | Mult
        | Div
        | Pow

data ArithmeticError = DivisionByZero | NegativePow deriving (Show)

instance Eq ArithmeticError where
  (==) :: ArithmeticError -> ArithmeticError -> Bool
  (==) DivisionByZero DivisionByZero = True
  (==) NegativePow    NegativePow    = True
  (==) _              _              = False

-- | Takes Expr and returns result of evaluation in Right or 
-- | ArithmeticalError if an error occured
eval :: Expr a -> Either ArithmeticError Int
eval (Value x                      ) = Right x
eval (BinaryOperation op left right) = binaryOperation op left right

binaryOperation :: Op -> Expr a -> Expr b -> Either ArithmeticError Int
binaryOperation op left right = case op of
  (Div) | isLeft leftValue            -> leftValue
        | isLeft rightValue           -> rightValue
        | fromRight 0 rightValue == 0 -> Left DivisionByZero
        | otherwise -> Right $ (quot) leftIntValue rightIntValue
  (Pow) | isLeft leftValue              -> leftValue
        | isLeft rightValue             -> rightValue
        | fromRight (-1) rightValue < 0 -> Left NegativePow
        | otherwise -> Right $ (^) leftIntValue rightIntValue
  _ | isLeft leftValue  -> leftValue
    | isLeft rightValue -> rightValue
    | otherwise         -> Right $ operation leftIntValue rightIntValue
 where 
  rightValue    = eval right
  leftValue     = eval left
  rightIntValue = fromRight 1 rightValue
  leftIntValue  = fromRight 1 leftValue
  operation     = case op of
    (Plus ) -> (+)
    (Minus) -> (-)
    (Mult ) -> (*)
    (Div  ) -> (quot)
    (Pow  ) -> (^)
