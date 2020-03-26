module Task6
  ( haroldFuncWHNF
  , nullWHNF
  )
where

-- | WHNF of distributivity (Left ("harold" ++ " hide " ++ "the " ++ "pain"))
haroldFuncWHNF :: (Either String b, Either String c)
haroldFuncWHNF =
  ( Left ("harold" ++ " hide " ++ "the " ++ "pain")
  , Left ("harold" ++ " hide " ++ "the " ++ "pain")
  )

-- | WHNF of null $ mapMaybe foo "pole chudes ochen' chudesno"
-- | Returns False
nullWHNF :: Bool
nullWHNF = False