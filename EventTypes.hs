module EventTypes where
import DateTypes
import Data.Maybe

data Event      = Event [EventProp]                     deriving (Eq, Show)

data EventProp  = DtStamp DateTime
                | Uid String
                | DtStart DateTime
                | DtEnd DateTime
                | Description String
                | Summary String
                | Location String                       
                | Organizer String
                deriving (Eq, Show)


{-
    Algebras and folds
-}

--
-- Event
--
type EventAlgebra e ep dt date time utc = ([ep] -> e, EventPropAlgebra ep dt date time utc)

foldEvent :: EventAlgebra e ep dt date time utc -> Event -> e
foldEvent (event, eventProp) = fold
    where
        fold (Event props) = event (map (foldEventProp eventProp) props)

-- id for Event
idEvent :: EventAlgebra Event EventProp DateTime Date Time TimeUTC
idEvent = (Event, idEventProp)

--
-- EventProp
--
type EventPropAlgebra ep dt date time utc =
    (
        (
            dt -> ep,
            String -> ep,
            dt -> ep,
            dt -> ep,
            String -> ep,
            String -> ep,
            String -> ep,
            String -> ep
        ),
        DateTimeAlgebra dt date time utc
    )

foldEventProp :: EventPropAlgebra ep dt date time utc -> EventProp -> ep
foldEventProp ((dtStamp, uid, dtStart, dtEnd, descr, summary, location, organizer), datetime) = fold
    where
        fold (DtStamp dt)       = dtStamp (foldDateTime datetime dt)
        fold (Uid str)          = uid str
        fold (DtStart dt)       = dtStart (foldDateTime datetime dt)
        fold (DtEnd dt)         = dtEnd (foldDateTime datetime dt)
        fold (Description str)  = descr str
        fold (Summary str)      = summary str
        fold (Location str)     = location str
        fold (Organizer str)    = organizer str

idEventProp :: EventPropAlgebra EventProp DateTime Date Time TimeUTC
idEventProp = (
        (
            DtStamp, Uid, DtStart, DtEnd, Description, Summary, Location, Organizer
        ),
        idDateTime
    )