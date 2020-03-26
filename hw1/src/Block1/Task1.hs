{-# LANGUAGE InstanceSigs #-}

module Block1.Task1
  ( afterDays
  , daysToParty
  , isWeekend
  , nextDay
  , Weekday(..)
  )
where

-- | Day of week
data Weekday = Monday
             | Tuesday
             | Wednesday
             | Thursday
             | Friday
             | Saturday
             | Sunday
             deriving (Show)

instance Eq Weekday where
  (==) :: Weekday -> Weekday -> Bool
  (==) lhs rhs = fromEnum lhs == fromEnum rhs

instance Enum Weekday where
  toEnum :: Int -> Weekday
  toEnum 1 = Monday
  toEnum 2 = Tuesday
  toEnum 3 = Wednesday
  toEnum 4 = Thursday
  toEnum 5 = Friday
  toEnum 6 = Saturday
  toEnum 7 = Sunday
  toEnum _ = error "The number of the day must be from 1 to 7"

  fromEnum :: Weekday -> Int
  fromEnum Monday    = 1
  fromEnum Tuesday   = 2
  fromEnum Wednesday = 3
  fromEnum Thursday  = 4
  fromEnum Friday    = 5
  fromEnum Saturday  = 6
  fromEnum Sunday    = 7

-- | Returns next day for given
nextDay :: Weekday -> Weekday
nextDay weekday = toEnum $ mod ((+ 1) (fromEnum weekday)) numberOfWeekdays

numberOfWeekdays :: Int
numberOfWeekdays = 7

-- | Takes a weekday and an offset, returns weekday after the offset
afterDays :: Weekday -> Int -> Weekday
afterDays weekday offset
  | offset >= 0 = toEnum $ (+) offset' $ fromEnum weekday
  | otherwise   = error "Offset must be non-negative number"
  where offset' = offset `mod` numberOfWeekdays

-- | Returns True is given weekday is weekend and False otherwise
isWeekend :: Weekday -> Bool
isWeekend Saturday = True
isWeekend Sunday   = True
isWeekend _        = False

-- | Returns amount of day before the closest friday
daysToParty :: Weekday -> Int
daysToParty weekday | differenceInDays >= 0 = differenceInDays
                    | otherwise = numberOfWeekdays + differenceInDays
  where differenceInDays = (-) (fromEnum Friday) $ fromEnum weekday
