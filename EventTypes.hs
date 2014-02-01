module EventTypes where
import DateTypes
import Data.Maybe

data Event      = Event [EventProp]                     deriving (Eq)

data EventProp  = DtStamp DateTime
                | Uid String
                | DtStart DateTime
                | DtEnd DateTime
                | Description String
                | Summary String
                | Location String                       deriving (Eq)


{-
    Algebras and folds
-}

--
-- Event
--
type EventAlgebra e ep dt date time year doubledigit digit utc = ([ep] -> e, EventPropAlgebra ep dt date time year doubledigit digit utc)

foldEvent :: EventAlgebra e ep dt date time year doubledigit digit utc -> Event -> e
foldEvent (event, eventProp) = fold
    where
        fold (Event props) = event (map (foldEventProp eventProp) props)

-- id for Event
idEvent :: EventAlgebra Event EventProp DateTime Date Time Year DoubleDigit Digit TimeUTC
idEvent = (Event, idEventProp)

--
-- EventProp
--
type EventPropAlgebra ep dt date time year doubledigit digit utc =
    (
        (
            dt -> ep,
            String -> ep,
            dt -> ep,
            dt -> ep,
            String -> ep,
            String -> ep,
            String -> ep
        ),
        DateTimeAlgebra dt date time year doubledigit digit utc
    )

foldEventProp :: EventPropAlgebra ep dt date time year doubledigit digit utc -> EventProp -> ep
foldEventProp ((dtStamp, uid, dtStart, dtEnd, descr, summary, location), datetime) = fold
    where
        fold (DtStamp dt)       = dtStamp (foldDateTime datetime dt)
        fold (Uid str)          = uid str
        fold (DtStart dt)       = dtStart (foldDateTime datetime dt)
        fold (DtEnd dt)         = dtEnd (foldDateTime datetime dt)
        fold (Description str)  = descr str
        fold (Summary str)      = summary str
        fold (Location str)     = location str

-- id for EventProp
idEventProp :: EventPropAlgebra EventProp DateTime Date Time Year DoubleDigit Digit TimeUTC
idEventProp = ((DtStamp, Uid, DtStart, DtEnd, Description, Summary, Location), idDateTime)

{-
    Instances
-}
instance Show Event where
    show (Event x) = "BEGIN:VEVENT\r\n" ++ (concat $ map show x) ++ "END:VEVENT\r\n"

instance Ord Event where
    -- The event that starts first is younger than the other
    e1 <= e2 = (startDate e1) <= (startDate e2)

instance Show EventProp where
    show (DtStamp t)    = "DTSTAMP:"    ++ printDateTime t   ++ "\r\n"
    show (Uid s)        = "UID:"        ++ s                 ++ "\r\n"
    show (DtStart t)    = "DTSTART:"    ++ printDateTime t   ++ "\r\n"
    show (DtEnd t)      = "DTEND:"      ++ printDateTime t   ++ "\r\n"
    show (Description s)= "DESCRIPTION:"++ s                 ++ "\r\n"
    show (Summary s)    = "SUMMARY:"    ++ s                 ++ "\r\n"
    show (Location s)   = "LOCATION:"   ++ s                 ++ "\r\n"

{-
    Fancy Printing
-}
shortEventDescription :: Event -> String
shortEventDescription e = start ++ " - " ++ end
    where
        nice = foldDateTime niceTime
        start = nice $ startDate e
        end = nice $ endDate e

{-
    Helper functions
-}
-- Extract the date from an EventProp
getDatePropAlg :: EventPropAlgebra (Maybe DateTime) DateTime Date Time Year DoubleDigit Digit TimeUTC
getDatePropAlg = (
        (
            Just,           -- DtStamp
            const Nothing,  -- Uid
            Just,           -- DtStart
            Just,           -- DtEnd
            const Nothing,  -- Description
            const Nothing,  -- Summary
            const Nothing   -- Location
        ),
        idDateTime
    )
getDateProp :: EventProp -> Maybe DateTime
getDateProp = foldEventProp getDatePropAlg

getPropString :: EventProp -> Maybe String
getPropString (Uid s)           = Just s
getPropString (Description s)   = Just s
getPropString (Summary s)       = Just s
getPropString (Location s)      = Just s
getPropString _ = Nothing


-- Whether an event starts on a day
isOnDay :: Int -> Event -> Bool
isOnDay day e = day >= startDay && day <= endDay
    where
        startDay = foldDateTime getDayAlg (startDate e)
        endDay   = foldDateTime getDayAlg (endDate e)

        getDayAlg :: DateTimeAlgebra Int Int Int Int Int Int Int
        getDayAlg = (const, (\_ _ d -> d, yToIntAlg, ddToIntAlg), (\h m s _ -> 0, ddToIntAlg, (0, 0)))



-- The starting date of an event
startDate :: Event -> DateTime
startDate (Event prop) = fromJust . getDateProp . head $ filter date prop
    where
        date (DtStart _) = True
        date _ = False

{-
-- This solution uses the Algebra, but I find it uglier than the other solution.

startDateAlg :: EventAlgebra DateTime (Maybe DateTime) DateTime Date Time Year DoubleDigit Digit TimeUTC
startDateAlg = (fromJust . head . filter (not . isNothing),
        (
            (
                const Nothing,  -- DtStamp
                const Nothing,  -- Uid
                Just,           -- DtStart
                const Nothing,  -- DtEnd
                const Nothing,  -- Description
                const Nothing,  -- Summary
                const Nothing   -- Location
            ),
            idDateTime
        )
    )

startDate :: Event -> DateTime
startDate = foldEvent startDateAlg-}

-- The date on which an event ends
endDate :: Event -> DateTime
endDate (Event prop) = fromJust . getDateProp . head $ filter date prop
    where
        date (DtEnd _) = True
        date _ = False

-- The summary of an event
summary :: Event -> String
summary (Event prop) = fromJust . getPropString . head $ filter f prop
    where
        f (Summary _) = True
        f _ = False

-- Whether the event is on-going on this date
isOnDate :: Event -> DateTime -> Bool
isOnDate event date = date >= start && date <= end
    where
        start = startDate event
        end = endDate event

-- Whether two events are overlapping
overlaps :: Event -> Event -> Bool
overlaps e1 e2 = not (start2 >= end1 || start1 >= end2)
    where
        start1 = startDate e1
        end1 = endDate e1

        start2 = startDate e2
        end2 = endDate e2

-- Get the duration time of an event in seconds
getDuration :: Event -> Integer
getDuration event = endDate event `dateDiff` startDate event

-- Whether an event is in a month
eventInMonth :: Int -> Int -> Event -> Bool
eventInMonth year month event = f start || f end
    where
        f = flip ((flip isInMonth) year) month
        start = startDate event
        end = endDate event
