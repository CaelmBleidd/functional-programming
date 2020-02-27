module Task6
  ( haroldFuncWHNF
  , nullWHNF
  )
where

haroldFuncWHNF :: (Either String b, Either String c)
haroldFuncWHNF = (Left ("harold" ++ " hide " ++ "the " ++ "pain"), Left ("harold" ++ " hide " ++ "the " ++ "pain"))

nullWHNF :: Bool
nullWHNF = False
