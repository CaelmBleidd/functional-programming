module Task5
  ( churchPlus
  , churchToInt
  , churchMult
  , intToChurch
  , succChurch
  , zero
  )
where

type Nat a = (a -> a) -> a -> a

-- | Zero in church numerals
zero :: Nat a
zero _ x = x

-- | Function successor for church numerals
succChurch :: Nat a -> Nat a
succChurch n f x = f $ n f x

-- | Compute a + b for two church numerals
churchPlus :: Nat a -> Nat a -> Nat a
churchPlus f x g y = f g $ x g y

-- | Compute a * b for two church numerals
churchMult :: Nat a -> Nat a -> Nat a
churchMult f g x = f $ g x

-- | Transform a churh numeral to Integer
churchToInt :: Nat Integer -> Integer
churchToInt f = f (+ 1) 0

-- | Transform an Integer to church numeral
intToChurch :: Integer -> Nat Integer
intToChurch number f x = intToChurch' f x number
 where
  intToChurch' :: (Integer -> Integer) -> Integer -> Integer -> Integer
  intToChurch' g k n | n < 0     = error "The argument must be non-negative"
                     | n == 0    = k
                     | otherwise = g $ intToChurch' g k (n - 1)
