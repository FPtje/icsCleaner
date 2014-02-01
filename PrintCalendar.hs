module PrintCalendar(showCalendar) where

import Text.PrettyPrint
import CalendarTypes
import EventTypes
import DateTypes

-- pretty version of a calendar
showCalendar :: Calendar -> String
showCalendar = render . foldCalendar prettyCalendar

-- Convert a showable to a Doc
pr :: Show a => a -> Doc
pr = text . show

-- Pretty calendar algebra
prettyCalendar :: CalendarAlgebra Doc Doc Doc Doc Doc Doc Doc Doc
prettyCalendar = (
    fCalendar, -- Calendar carrier
    ( -- Calendar prop
        fVersion,
        fProdid,
        fMethod
    ),
    ( -- Event
        fEvent,
        ( -- Event prop
            (
                fDtStamp,
                fUid,
                fDtStart,
                fDtEnd,
                fDescription,
                fSummary,
                fLocation,
                fOrganizer,
                fClass,
                fPriority
            ),
            (-- DateTime
                fdatetime,
                fdate,
                ftime,
                futc
            )
        )
    )

 ) where
    fCalendar    props evs   = text "BEGIN:VCALENDAR" $$ vcat props $$ vcat evs $$ text "END:VCALENDAR"
    fVersion                 = text "VERSION:2.0"
    fProdid      val         = text "PRODID:" <> pr val
    fMethod      val         = text "METHOD:" <> pr val
    fEvent       eps         = text "BEGIN:VEVENT" $$ vcat eps $$ text "END:VEVENT"
    fDtStamp     val         = text "DTSTAMP:"     <> val
    fUid         val         = text "UID:"         <> text val
    fDtStart     val         = text "DTSTART:"     <> val
    fDtEnd       val         = text "DTEND:"       <> val
    fDescription val         = text "DESCRIPTION:" <> text val
    fSummary     val         = text "SUMMARY:"     <> text val
    fLocation    val         = text "LOCATION:"    <> text val
    fOrganizer   val         = text "ORGANIZER:"   <> text val
    fClass       val         = text "CLASS:"       <> text val
    fPriority    val         = text "PRIORITY:"    <> pr val
    fdatetime    d t         = d    <> text "T"    <> t
    fdate        y m d       = pr y <> pr m        <> pr d
    ftime        h m s u     = pr h <> pr m        <> pr s     <> u
    futc         UTC         = text "Z"
    futc         TimeLocal   = text ""