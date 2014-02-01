module PrintCalendar where

import Text.PrettyPrint
import CalendarTypes
import EventTypes
import DateTypes

prettyCalendar :: CalendarAlgebra Doc Doc Doc Doc Doc Doc Doc Doc
prettyCalendar = (
    fCalendar, -- calendar carrier
    ( -- calendar prop
        fVersion,
        fProdid,
        fMethod
    ),
    (
        fEvent,
        (
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
            (
                fdatetime,
                fdate,
                ftime,
                futc
            )
        )
    )

 ) where
    fCalendar     = undefined
    fVersion      = undefined
    fProdid       = undefined
    fMethod       = undefined
    fEvent        = undefined
    fDtStamp      = undefined
    fUid          = undefined
    fDtStart      = undefined
    fDtEnd        = undefined
    fDescription  = undefined
    fSummary      = undefined
    fLocation     = undefined
    fOrganizer    = undefined
    fClass        = undefined
    fPriority     = undefined
    fdatetime     = undefined
    fdate         = undefined
    ftime         = undefined
    futc          = undefined