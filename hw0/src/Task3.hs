module Task3
  ( composition
  , contraction
  , identity
  , permutation
  , s
  )
where

-- | S combinator implementation
s :: (a -> b -> c) -> (a -> b) -> a -> c
s f g x = f x $ g x

-- | B combinator implementation
composition :: (b -> c) -> (a -> b) -> a -> c
composition = s (const s) const

-- | I combinator implementation
identity :: a -> a
identity = s const const

-- | W combinator implementation
contraction :: (a -> a -> b) -> a -> b
contraction = s s $ const $ s const const

-- | C combinator implementation
permutation :: (a -> b -> c) -> b -> a -> c
permutation = s ((s (const s) const) (s (const s) const) s) $ const const
