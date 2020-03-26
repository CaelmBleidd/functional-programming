module Block4.Task1
  ( stringSum
  )
where

import           Text.Read

-- | Takes String and returns Just sum of all Int in the line
-- | or Nothing if there is value cannot be parsed
stringSum :: String -> Maybe Int
stringSum list = getResult $ traverse readMaybe (words list)
 where
  getResult :: Maybe [Int] -> Maybe Int
  getResult Nothing  = Nothing
  getResult (Just x) = Just (sum x)