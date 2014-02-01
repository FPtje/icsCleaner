module CalendarTypes where
--Data Types for the calendar
import DateTypes
import EventTypes

data Calendar   = Calendar [Calprop] [Event]            deriving (Eq)

data Calprop    = Version
                | Prodid String
                | Method String
                deriving (Eq)

{-
	Algebras and folds
-}

-- Calendar
type CalendarAlgebra cal calprop e ep dt date time utc = (
		[calprop] -> [e] -> cal,
		CalpropAlgebra calprop,
		EventAlgebra e ep dt date time utc
	)

foldCalendar :: CalendarAlgebra cal calprop e ep dt date time utc -> Calendar -> cal
foldCalendar (cal, prop, event) = fold
	where
		fold (Calendar props events) = cal (map (foldCalprop prop) props) (map (foldEvent event) events)

-- id for Calendar
idCalendar :: CalendarAlgebra Calendar Calprop Event EventProp DateTime Date Time TimeUTC
idCalendar = (Calendar, idCalProp, idEvent)

-- Calendar property
type CalpropAlgebra c = (c, String -> c, String -> c)

foldCalprop :: CalpropAlgebra c -> Calprop -> c
foldCalprop (v, p, m) = fold
	where
		fold Version = v
		fold (Prodid str) = p str
		fold (Method str) = m str

-- id for Calendar property
idCalProp :: CalpropAlgebra Calprop
idCalProp = (Version, Prodid, Method)

{-
	Instances
-}
instance Show Calendar where
    show (Calendar xs ys) = "BEGIN:VCALENDAR\r\n" ++ concatMap show xs ++ concatMap show ys ++ "END:VCALENDAR\r\n"

instance Show Calprop where
    show Version   = "VERSION:2.0\r\n"
    show (Prodid s)= "PRODID:" ++ s ++ "\r\n"
    show (Method s)= "METHOD:" ++ s ++ "\r\n"

-- Convert a Calendar to a string
printCalendar :: Calendar -> String
printCalendar = show

{-
	Calendar operations
-}

-- Get the amount of events in a calendar
eventCount :: Calendar -> Int
eventCount (Calendar _ es) = length es
