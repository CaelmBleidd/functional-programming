{-# LANGUAGE InstanceSigs #-}
module Block1.Task2
  ( (+)
  , (-)
  , (*)
  , (==)
  , (<=)
  , fromInteger
  , Block1.Task2.isEven
  , Block1.Task2.div
  , Block1.Task2.mod
  , Block1.Task2.toInteger
  , Nat(..)
  , Natural
  )
where

data Nat = Z | S Nat deriving(Show)

-- | Num implementation for Nat
-- | Nut can be constructed only from non-negative numbers
-- | Attemption to create Nat from negative number will cause error
-- | (-) will cause error if second argument is greater the the first one
instance Num Nat where
  (+) :: Nat -> Nat -> Nat
  (+) first second = add first second

  (-) :: Nat -> Nat -> Nat
  (-) first second = subt first second

  (*) :: Nat -> Nat -> Nat
  (*) first second = mult first second

  signum :: Nat -> Nat
  signum number = natSignum number

  abs :: Nat -> Nat
  abs number = number

  fromInteger :: Integer -> Nat
  fromInteger number = intToNat number

instance Eq Nat where
  (==) :: Nat -> Nat -> Bool
  (==) lhs rhs = equals lhs rhs

instance Ord Nat where
  (<=) :: Nat -> Nat -> Bool
  (<=) lhs rhs = lessOrEquals lhs rhs

class (Ord a, Num a) => Natural a where
  isEven :: a -> Bool
  div :: a -> a -> a
  mod :: a -> a -> a
  toInteger :: a -> Integer

instance Natural Nat where
  isEven number = isEvenNat number
  div divident divider = division divident divider
  mod divident divider = modDivision divident divider
  toInteger number = natToInt number

natSignum :: Nat -> Nat
natSignum Z = Z
natSignum _ = S Z

natToInt :: Nat -> Integer
natToInt Z     = 0
natToInt (S n) = (+) 1 (natToInt n)

intToNat :: Integer -> Nat
intToNat x | x == 0    = Z
           | x > 0     = S $ intToNat $ x - 1
           | otherwise = error "Given number must be non negative"

add :: Nat -> Nat -> Nat
add x Z     = x
add x (S n) = add (S x) n

mult :: Nat -> Nat -> Nat
mult _ Z     = Z
mult x (S y) = add x (mult x y)

subt :: Nat -> Nat -> Nat
subt x Z = x
subt Z _ = error errorText
 where
  errorText = "The subtract value cannot be greater than the decrease value"
subt (S x) (S y) = subt x y

equals :: Nat -> Nat -> Bool
equals Z     Z     = True
equals Z     _     = False
equals _     Z     = False
equals (S x) (S y) = equals x y

greater :: Nat -> Nat -> Bool
greater Z     _     = False
greater (S _) Z     = True
greater (S x) (S y) = greater x y

greaterOrEquals :: Nat -> Nat -> Bool
greaterOrEquals x y = greater x y || equals x y

less :: Nat -> Nat -> Bool
less x y = not $ greaterOrEquals x y

lessOrEquals :: Nat -> Nat -> Bool
lessOrEquals x y = not $ greater x y

isEvenNat :: Nat -> Bool
isEvenNat Z         = True
isEvenNat (S Z    ) = False
isEvenNat (S (S x)) = isEvenNat x

division :: Nat -> Nat -> Nat
division divident divider | equals divident Z = error "Divide by zero"
                          | otherwise         = division' divident divider

division' :: Nat -> Nat -> Nat
division' divident divider
  | greater divider divident = Z
  | otherwise                = S (division' (subt divident divider) divider)

modDivision :: Nat -> Nat -> Nat
modDivision divident divider | equals divident Z = error "Divide by zero"
                             | otherwise         = modDivision' divident divider

modDivision' :: Nat -> Nat -> Nat
modDivision' divident divider
  | less divident divider = divident
  | otherwise             = modDivision' (subt divident divider) divider
