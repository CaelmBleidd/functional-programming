{-# LANGUAGE TypeOperators #-}

module Task1
  ( associator
  , distributivity
  , eitherAssoc
  )
where

-- | Takes either from an element a and pair (b, c) and returns 
-- | pair (Either a b, Either b c)
distributivity :: Either a (b, c) -> (Either a b, Either a c)
distributivity (Left  a     ) = (Left a, Left a)
distributivity (Right (b, c)) = (Right b, Right c)

-- | Changes associativity
associator :: (a, (b, c)) -> ((a, b), c)
associator (a, (b, c)) = ((a, b), c)

type (<->) a b = (a -> b, b -> a)

eitherAssocForward :: Either a (Either b c) -> Either (Either a b) c
eitherAssocForward (Left  a        ) = Left $ Left a
eitherAssocForward (Right (Left  b)) = Left $ Right b
eitherAssocForward (Right (Right c)) = Right c

eitherAssocBack :: Either (Either a b) c -> Either a (Either b c)
eitherAssocBack (Left  (Left  a)) = Left a
eitherAssocBack (Left  (Right b)) = Right $ Left b
eitherAssocBack (Right c        ) = Right $ Right c

-- | 
eitherAssoc :: Either a (Either b c) <-> Either (Either a b) c
eitherAssoc = (eitherAssocForward, eitherAssocBack)
