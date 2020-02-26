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

-- (Either a (Neg a) -> Void) -> Void
-- f :: (Either a (Neg a) -> Void)
excludedNeg :: Neg (Neg (Either a (Neg a)))
excludedNeg = undefined

-- impossible
pierce :: ((a -> b) -> a) -> a
pierce = undefined

-- impossible
doubleNegElim :: Neg (Neg a) -> a
doubleNegElim = undefined

thirdNegElim :: Neg (Neg (Neg a)) -> Neg a
thirdNegElim = undefined
