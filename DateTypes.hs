module DateTypes where
import Data.Maybe
import qualified Data.Time.Calendar as Cal

{-
	Data types
-}

data DateTime = DateTime Date Time deriving (Show, Eq)

data Date = Date Year Month Day deriving (Show, Eq)
data Time = Time Hour Minute Second TimeUTC deriving (Show, Eq)

data Year = Year Digit Digit Digit Digit deriving (Eq)

-- One data structure for months, days, hours, minutes and seconds
-- They can be thrown together since their syntax is exactly the same (digit digit)
data DoubleDigit = DoubleDigit Digit Digit deriving (Eq)

-- These are all double digits
type Month = DoubleDigit
type Day = DoubleDigit
type Hour = DoubleDigit
type Minute = DoubleDigit
type Second = DoubleDigit

-- UTC or local time
data TimeUTC = UTC | TimeLocal deriving (Eq)

-- Use an exact digit. Using Char as definition would be too broad
data Digit = Zero | One | Two | Three | Four | Five | Six | Seven | Eight | Nine deriving (Eq, Ord, Enum)

{-
	Algebras and folds
-}

--
-- DateTime
--
type DateTimeAlgebra dt date time y dd d utc = (date -> time -> dt, DateAlgebra date y dd d, TimeAlgebra time dd d utc)

foldDateTime (dt, d, t) = fold
	where
		fold (DateTime date time) = dt (foldDate d date) (foldTime t time)

niceDateTimePrint :: DateTimeAlgebra String String String String String Char String
niceDateTimePrint = (\d t -> d ++ " " ++ t, niceDatePrint, niceTimePrint)

-- niceTime for a string
niceTime :: DateTimeAlgebra String String String String String Char String
niceTime = (flip const, niceDatePrint, niceTimePrint)

-- DateTime diff in seconds
dateDiffAlg :: DateTimeAlgebra (Integer -> Integer) Integer Integer Int Int Int TimeUTC
dateDiffAlg = (\date time -> abs . ((-) (date * 86400 + time)),
		-- Date
		dateDays,
		-- Time
		timeSeconds
	)

dateDiff :: DateTime -> DateTime -> Integer
dateDiff d1 d2 = foldDateTime dateDiffAlg d1 $ foldDateTime dateTimeSeconds d2
	where
		dateTimeSeconds :: DateTimeAlgebra Integer Integer Integer Int Int Int TimeUTC
		dateTimeSeconds = (\date time -> (date * 86400 + time), dateDays, timeSeconds)

-- Whether a DateTime is in a certain year/month
isInMonthAlg :: DateTimeAlgebra (Int -> Int -> Bool) (Int -> Int -> Bool) Integer Int Int Int TimeUTC
isInMonthAlg = (const,
	(\year month _ y m -> y == year && m == month, yToIntAlg, ddToIntAlg), -- Date
	timeSeconds -- Time
	)

isInMonth :: DateTime -> Int -> Int -> Bool
isInMonth = foldDateTime isInMonthAlg

-- id for DateTime
idDateTime :: DateTimeAlgebra DateTime Date Time Year DoubleDigit Digit TimeUTC
idDateTime = (DateTime, idDate, idTime)

--
-- Date
--
type DateAlgebra date y dd d = (y -> dd -> dd -> date, YearAlgebra y d, DoubleDigitAlgebra dd d)

foldDate :: DateAlgebra date y dd d -> Date -> date
foldDate (date, year, dd) = fold
	where
		fold (Date y m d) = date (foldYear year y) (foldDoubleDigit dd m) (foldDoubleDigit dd d)

-- print the date in a nice format
niceDatePrint :: DateAlgebra String String String Char
niceDatePrint = (\y m d -> y ++ "/" ++ m ++ "/" ++ d, yToStringAlg, ddToStringAlg)

-- A date to amount of days
dateDays :: DateAlgebra Integer Int Int Int
dateDays = (\y m d -> Cal.diffDays (Cal.fromGregorian (toInteger y) m d) (Cal.fromGregorian 0 0 0), yToIntAlg, ddToIntAlg)

-- id for date
idDate :: DateAlgebra Date Year DoubleDigit Digit
idDate = (Date, idYear, idDoubleDigit)

--
-- Time
--
type TimeAlgebra t dd d utc = (dd -> dd -> dd -> utc -> t, DoubleDigitAlgebra dd d, UTCAlgebra utc)

foldTime (t, dd, utc) = fold
	where
		fold (Time h m s u) = t (foldDoubleDigit dd h) (foldDoubleDigit dd m) (foldDoubleDigit dd s) (foldUTC utc u)

-- print the time in a nice format
niceTimePrint :: TimeAlgebra String String Char String
niceTimePrint = (\h m s utc -> h ++ ":" ++ m ++ ":" ++ s ++ utc, ddToStringAlg, ("", ""))

-- time in seconds
timeSeconds :: TimeAlgebra Integer Int Int TimeUTC
timeSeconds = (\h m s _ -> toInteger $ h * 60 * 60 + m * 60 + s, ddToIntAlg, (UTC, TimeLocal))

-- id for time
idTime :: TimeAlgebra Time DoubleDigit Digit TimeUTC
idTime = (Time, idDoubleDigit, (UTC, TimeLocal))

--
-- Year
--
type YearAlgebra y d = (d -> d -> d -> d -> y, DigitAlgebra d)

foldYear :: YearAlgebra y d -> Year -> y
foldYear (y, d) = fold
	where
		fold (Year d1 d2 d3 d4) = y (foldDigit d d1) (foldDigit d d2) (foldDigit d d3) (foldDigit d d4)

-- Convert a year to int
yToIntAlg :: YearAlgebra Int Int
yToIntAlg = (\d1 d2 d3 d4 -> d1 * 1000 + d2 * 100 + d3 * 10 + d4, dToIntAlg)

-- year to string
yToStringAlg :: YearAlgebra String Char
yToStringAlg = (\d1 d2 d3 d4 -> d1 : d2 : d3 : d4 : [], dToCharAlg)

-- year id
idYear :: YearAlgebra Year Digit
idYear = (Year, idDigit)

--
-- DoubleDigit
--
type DoubleDigitAlgebra dd d =  (d -> d -> dd, DigitAlgebra d)

foldDoubleDigit :: DoubleDigitAlgebra dd d -> DoubleDigit -> dd
foldDoubleDigit (dd, d) = fold
	where
		fold (DoubleDigit d1 d2) = dd (foldDigit d d1) (foldDigit d d2)

-- Convert a DoubleDigit to int
ddToIntAlg :: DoubleDigitAlgebra Int Int
ddToIntAlg = (\d1 d2 -> d1 * 10 + d2, dToIntAlg)

-- DoubleDigit to string
ddToStringAlg :: DoubleDigitAlgebra String Char
ddToStringAlg = (\d1 d2 -> d1 : d2 : [], dToCharAlg)

-- id for DoubleDigit
idDoubleDigit :: DoubleDigitAlgebra DoubleDigit Digit
idDoubleDigit = (DoubleDigit, idDigit)

--
-- Digit
--
type DigitAlgebra d = (d, d, d, d, d, d, d, d, d, d)

foldDigit :: DigitAlgebra d -> Digit -> d
foldDigit (d0,  _,  _,  _,  _,  _,  _,  _,  _,  _) Zero	 = d0
foldDigit ( _, d1,  _,  _,  _,  _,  _,  _,  _,  _) One	 = d1
foldDigit ( _,  _, d2,  _,  _,  _,  _,  _,  _,  _) Two	 = d2
foldDigit ( _,  _,  _, d3,  _,  _,  _,  _,  _,  _) Three = d3
foldDigit ( _,  _,  _,  _, d4,  _,  _,  _,  _,  _) Four	 = d4
foldDigit ( _,  _,  _,  _,  _, d5,  _,  _,  _,  _) Five	 = d5
foldDigit ( _,  _,  _,  _,  _,  _, d6,  _,  _,  _) Six	 = d6
foldDigit ( _,  _,  _,  _,  _,  _,  _, d7,  _,  _) Seven = d7
foldDigit ( _,  _,  _,  _,  _,  _,  _,  _, d8,  _) Eight = d8
foldDigit ( _,  _,  _,  _,  _,  _,  _,  _,  _, d9) Nine	 = d9

-- Convert a digit to an Int
dToIntAlg :: DigitAlgebra Int
dToIntAlg = (0,1,2,3,4,5,6,7,8,9)

-- Convert Digit to Char
dToCharAlg :: DigitAlgebra Char
dToCharAlg = ('0', '1', '2', '3', '4', '5', '6', '7', '8', '9')

-- id for Digit
idDigit :: DigitAlgebra Digit
idDigit = (Zero, One, Two, Three, Four, Five, Six, Seven, Eight, Nine)

--
-- UTC
--
type UTCAlgebra u = (u, u)

foldUTC :: UTCAlgebra u -> TimeUTC -> u
foldUTC (u1, u2) = fold
	where
		fold UTC = u1
		fold TimeLocal = u2


{-
	Conversion functions
-}

-- Helper function to convert a Char to a Digit
chrToDigit :: Char -> Digit
chrToDigit '0' = Zero
chrToDigit '1' = One
chrToDigit '2' = Two
chrToDigit '3' = Three
chrToDigit '4' = Four
chrToDigit '5' = Five
chrToDigit '6' = Six
chrToDigit '7' = Seven
chrToDigit '8' = Eight
chrToDigit '9' = Nine
chrToDigit _ = error "Invalid digit"

{-
	Instances
-}
instance Show Digit where
	show Zero	= "0"
	show One	= "1"
	show Two	= "2"
	show Three	= "3"
	show Four	= "4"
	show Five	= "5"
	show Six	= "6"
	show Seven	= "7"
	show Eight	= "8"
	show Nine	= "9"

instance Show TimeUTC where
	show UTC = "Z"
	show TimeLocal = ""

instance Show DoubleDigit where
	show = foldDoubleDigit ddToStringAlg --(DoubleDigit d1 d2) = (show d1) ++ (show d2)

instance Show Year where
	show (Year d1 d2 d3 d4) = (show d1) ++ (show d2) ++ (show d3) ++ (show d4)

instance Ord DateTime where
	(DateTime date1 time1) <= (DateTime date2 time2) = date1 < date2 || date1 == date2 && time1 <= time2

instance Ord Date where
	(Date y1 m1 d1) <= (Date y2 m2 d2) = y1 < y2 || y1 == y2 && m1 < m2 || y1 == y2 && m1 == m2 && d1 <= d2

instance Ord Time where
	(Time h1 m1 s1 _) <= (Time h2 m2 s2 _) = h1 < h2 || h1 == h2 && m1 < m2 || h1 == h2 && m1 == m2 && s1 <= s2

instance Ord Year where
	y1 <= y2 = yearInt y1 <= yearInt y2

instance Ord DoubleDigit where
	(DoubleDigit d1 d2) <= (DoubleDigit d3 d4) = d1 < d3 || d1 == d3 && d2 <= d4

-- Convert a Digit to Int
digitInt :: Digit -> Int
digitInt Zero	= 0
digitInt One	= 1
digitInt Two	= 2
digitInt Three	= 3
digitInt Four	= 4
digitInt Five	= 5
digitInt Six	= 6
digitInt Seven	= 7
digitInt Eight	= 8
digitInt Nine	= 9

-- Double digit to int
doubledigitInt :: DoubleDigit -> Int
doubledigitInt (DoubleDigit d1 d2) = (digitInt d1) * 10 + (digitInt d2)

-- Year to int
yearInt :: Year -> Int
yearInt (Year d1 d2 d3 d4) = (digitInt d1) * 1000 + (digitInt d2) * 100 + (digitInt d3) * 10 + (digitInt d4)

{-
	Printing
-}
printDateTime :: DateTime -> String
printDateTime (DateTime date time) = (printDate date) ++ "T" ++ (printTime time)

printDate :: Date -> String
printDate (Date year month day) = (show year) ++ (show month) ++ (show day)

printTime :: Time -> String
printTime (Time hour minute second timeutc) = (show hour) ++ (show minute) ++ (show second) ++ (show timeutc)

{-
	Checking dates and times
-}
checkDate :: Date -> Bool
checkDate = foldDate (isValid, yToIntAlg, ddToIntAlg)
	where
		isValid y m d = (not . isNothing) $ Cal.fromGregorianValid (toInteger y) m d

checkTime :: Time -> Bool
checkTime = foldTime (\h m s _-> h < 24 && m < 60 && s < 60, ddToIntAlg, (0, 1))

checkDateTime :: DateTime -> Bool
checkDateTime (DateTime date time) = checkDate date && checkTime time
