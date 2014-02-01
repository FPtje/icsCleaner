module AntiDuplicate where

import CalendarTypes
import EventTypes
import DateTypes

antiDuplicate :: CalendarAlgebra Calendar Calprop Event EventProp DateTime Date Time TimeUTC
antiDuplicate = (
	Calendar, 
	idCalProp, 
	idEvent
 )