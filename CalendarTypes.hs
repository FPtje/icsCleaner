module CalendarTypes where
--Data Types for the calendar
import DateTypes
import EventTypes

data Calendar   = Calendar [Calprop] [Event]            deriving (Eq)

data Calprop    = Version
                | Prodid String                         deriving (Eq)

{-
	Algebras and folds
-}

-- Calendar
type CalendarAlgebra cal calprop e ep dt date time year doubledigit digit utc = (
		[calprop] -> [e] -> cal,
		CalpropAlgebra calprop,
		EventAlgebra e ep dt date time year doubledigit digit utc
	)

foldCalendar :: CalendarAlgebra cal calprop e ep dt date time year doubledigit digit utc -> Calendar -> cal
foldCalendar (cal, prop, event) = fold
	where
		fold (Calendar props events) = cal (map (foldCalprop prop) props) (map (foldEvent event) events)

-- id for Calendar
idCalendar :: CalendarAlgebra Calendar Calprop Event EventProp DateTime Date Time Year DoubleDigit Digit TimeUTC
idCalendar = (Calendar, idCalProp, idEvent)

-- Calendar property
type CalpropAlgebra c = (c, String -> c)

foldCalprop :: CalpropAlgebra c -> Calprop -> c
foldCalprop (v, p) = fold
	where
		fold Version = v
		fold (Prodid str) = p str

-- id for Calendar property
idCalProp :: CalpropAlgebra Calprop
idCalProp = (Version, Prodid)

{-
	Instances
-}
instance Show Calendar where
    show (Calendar xs ys) = "BEGIN:VCALENDAR\r\n" ++ concatMap show xs ++ concatMap show ys ++ "END:VCALENDAR\r\n"

instance Show Calprop where
    show Version   = "VERSION:2.0\r\n"
    show (Prodid s)= "PRODID:" ++ s ++ "\r\n"

-- Convert a Calendar to a string
printCalendar :: Calendar -> String
printCalendar = show

{-
	Calendar operations
-}

-- Get the amount of events in a calendar
eventCount :: Calendar -> Int
eventCount (Calendar _ es) = length es

-- Get the events on a certain date and time
dateEvents :: Calendar -> DateTime -> [Event]
dateEvents (Calendar _ events) dateTime = filter ((flip isOnDate) dateTime) events

-- Get overlapping events. Returns a list of event pairs that overlap.
overlappingEventsAlg :: CalendarAlgebra [(Event, Event)] Calprop Event EventProp DateTime Date Time Year DoubleDigit Digit TimeUTC
overlappingEventsAlg = (\_ events -> [(x, y) | x <- events, y <- events, x `overlaps` y, x /= y],
	idCalProp,
	idEvent)

overlappingEvents :: Calendar -> [(Event, Event)]
overlappingEvents = foldCalendar overlappingEventsAlg

-- get events that have a certain summary
findBySummary :: Calendar -> String -> [Event]
findBySummary cal summ = foldCalendar (\_ -> filter $ (summ ==) . summary, idCalProp, idEvent) cal

-- Find the total time spent on all events with the given summary. The answer is in seconds.
getSpentTimeOn :: Calendar -> String -> Integer
getSpentTimeOn cal summ = foldl (+) 0 $ map getDuration events
	where
		events = findBySummary cal summ
