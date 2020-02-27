module Task2
  ( doubleNeg
  , doubleNegElim
  , excludedNeg
  , pierce
  , thirdNegElim
)
where

import           Data.Void                      ( Void )

type Neg a = a -> Void

doubleNeg :: a -> Neg (Neg a)
doubleNeg f x = x f

excludedNeg :: Neg (Neg (Either a (Neg a)))
excludedNeg = \f -> f $ Right $ f . Left

-- This expression has no proof in IL
-- therefore it's an uninhabited type
pierce :: ((a -> b) -> a) -> a
pierce = undefined

-- This expression has no proof in IL
-- therefore it's an uninhabited type
doubleNegElim :: Neg (Neg a) -> a
doubleNegElim = undefined

thirdNegElim :: Neg (Neg (Neg a)) -> Neg a
thirdNegElim = controposition doubleNeg
  where 
    controposition :: (a -> b) -> (Neg b -> Neg a)
    controposition f g a = g $ f a
