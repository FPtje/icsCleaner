module Printing where

import Text.PrettyPrint
import Data.List
import qualified Data.Time.Calendar as Cal
import CalendarTypes
import DateTypes
import EventTypes


--newtype MonthLayout = MonthLayout [[]]

ppMonth :: Int -> Int -> Calendar -> String
ppMonth year month cal = render $ printEvents evs year month
	where
		evs = getEventsInMonth cal year month

-- Get the sorted events in a month
getEventsInMonthAlg :: CalendarAlgebra (Int -> Int -> [Event]) Calprop Event EventProp DateTime Date Time Year DoubleDigit Digit TimeUTC
getEventsInMonthAlg = (\_ es year month -> filter (eventInMonth year month) es,
	idCalProp,
	idEvent)

getEventsInMonth :: Calendar -> Int -> Int -> [Event]
getEventsInMonth = foldCalendar getEventsInMonthAlg

-- Get events happening on certain day (currently starting date only)
getEventsOnDay :: Int -> [Event] -> [Event]
getEventsOnDay day = filter (isOnDay day)

-- get events on interval
getEventsOnInterval :: Int -> Int -> [Event] -> [[Event]]
getEventsOnInterval start end ev = map ((flip getEventsOnDay) ev) [start..end]

-- Get the maximum amount of events happening in an interval of days
getMaxEventsDayInterval :: Int -> Int -> [Event] -> Int
getMaxEventsDayInterval start end ev = maximum $ map (length . (flip getEventsOnDay) ev) [start..end]


{-
	Fancy text things
-}
dayTop :: Doc
dayTop = text "+---------------------"

dayHeader :: Int -> Doc
dayHeader i = char '|' <+> int i <> char '.' <> text (replicate numCount ' ')
	where
		numCount = if i < 10 then 18 else 17

dayTops :: Int -> Int -> Doc
dayTops start to = hcat (map dayHeader [start .. to]) <> char '|'

dayLine :: Event -> Doc
dayLine ev = text "|" <+> text (shortEventDescription ev)

emptyDayLine :: Doc
emptyDayLine = text "|                    "

calendarRow :: Int -> Int -> [Event] -> Doc
calendarRow start end events = dayTops start end $$ row evs $$ hcat (replicate (end - start + 1) dayTop) <> char '+'
	where
		evs = getEventsOnInterval start end events
		maxRows = getMaxEventsDayInterval start end events

		row :: [[Event]] -> Doc
		row evs | all null evs = empty
				| otherwise  = hsep (map print evs) <+> char '|' $$ row (map tail' evs)

		print :: [Event] -> Doc
		print [] 		= emptyDayLine
		print (x : xs) 	= dayLine x

		tail' :: [Event] -> [Event]
		tail' [] = []
		tail' (x : xs) = xs

printEvents :: [Event] -> Int -> Int -> Doc
printEvents evs year month = (hcat $ replicate 7 dayTop) <> char '+' $$ doPrint 1
	where
		days = Cal.gregorianMonthLength (toInteger year) month

		doPrint :: Int -> Doc
		doPrint i | i <= days = calendarRow i ((i + 6) `min` days) evs $$ doPrint (i + 7)
		doPrint i | otherwise = empty
