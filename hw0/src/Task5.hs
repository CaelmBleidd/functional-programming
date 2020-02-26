module Task5
  ( churchPlus
  , churchToInt
  , succChurch
  , zero
  )
where

type Nat a = (a -> a) -> a -> a

zero :: Nat a
zero _ x = x

succChurch :: Nat a -> Nat a
succChurch n f x = f (n f x)

churchPlus :: Nat a -> Nat a -> Nat a
churchPlus f x g y = f g (x g y)

churchMult :: Nat a -> Nat a -> Nat a
churchMult f g x = f (g x)

churchToInt :: Nat Integer -> Integer
churchToInt f = f (+ 1) 0

intToChurch :: Integer -> Nat Integer
intToChurch number f x = intToChurch' f x number
 where
  intToChurch' :: (Integer -> Integer) -> Integer -> Integer -> Integer
  intToChurch' g k n | n < 0     = error "The argument must be non-negative"
                     | n == 0    = k
                     | otherwise = g $ intToChurch' g k (n - 1)


