module DateTypes where
import Data.Maybe
import qualified Data.Time.Calendar as Cal

{-
	Data types
-}

data DateTime = DateTime Date Time deriving (Show, Eq)

data Date = Date Year Month Day deriving (Show, Eq)
data Time = Time Hour Minute Second TimeUTC deriving (Show, Eq)

-- These are all double digits
type Year   = Int
type Month  = Int
type Day    = Int
type Hour   = Int
type Minute = Int
type Second = Int

-- UTC or local time
data TimeUTC = UTC | TimeLocal deriving (Eq)


instance Show TimeUTC where
	show UTC = "Z"
	show TimeLocal = ""

{-
	Algebras and folds
-}

--
-- DateTime
--
type DateTimeAlgebra dt date time utc = (date -> time -> dt, Year -> Month -> Day -> date, Hour -> Minute -> Second -> utc -> time, TimeUTC -> utc)

foldDateTime (datetime, date, time, utc) = fold
	where

		fold (DateTime (Date yy mm dd) (Time h m s u)) = datetime (date yy mm dd) (time h m s (utc u))

idDateTime :: DateTimeAlgebra DateTime Date Time TimeUTC
idDateTime = (DateTime, Date, Time, id)

strDateTime :: DateTimeAlgebra String String String String
strDateTime = (
	\d t -> d ++ "t" ++ t,
	\yy mm dd -> (show yy) ++ (show mm) ++ (show dd),
	\h m s u -> (show h) ++ (show m) ++ (show s) ++ u,
	\u -> if u == UTC then "Z" else ""
	)