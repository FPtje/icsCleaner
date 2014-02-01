module AntiDuplicate where

import CalendarTypes
import EventTypes
import DateTypes
import Data.List
import qualified Data.Map as M


antiDuplicate :: CalendarAlgebra Calendar Calprop Event EventProp DateTime Date Time TimeUTC
antiDuplicate = (
	\props evs -> Calendar props (remDup evs),
	idCalProp, 
	idEvent
 ) where
	remDup :: [Event] -> [Event]
	remDup evs = map snd . M.toList $ foldr ($) M.empty (map checkDup evs)

	checkDup :: Event -> M.Map String Event -> M.Map String Event
	checkDup e evs = let id = getEventID e in
		if id `M.member` evs then
			evs
		else
			M.insert id e evs

	eventPropID :: EventPropAlgebra String String String String String
	eventPropID = (
	        (
	            const "", -- DtStamp
	            const "", -- Uid
	            id,       -- DtStart
	            id,       -- DtEnd
	            const "", -- Description
	            id,       -- Summary
	            const "", -- Location
	            const "", -- Organizer
	            const "", -- Class
	            const ""  -- Priority
	        ),
	        strDateTime
	    )

	eventIdentifier :: EventAlgebra String String String String String String
	eventIdentifier = (concat . sort, eventPropID)

	getEventID :: Event -> String
	getEventID = foldEvent eventIdentifier