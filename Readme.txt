Authors:
	- Falco Peijnenburg (3749002)
	- Jorrit Krapels 	(3810992)

Used packages:
- uu-tc
- Text.PrettyPrint

No function names were given in assignment 11. Here are the function names and locations:
- Amount of events in a calendar, in CalendarTypes.hs:
	eventCount :: Calendar -> Int
- Events happening on given date and time: dateEvents in CalendarTypes.hs
	dateEvents :: Calendar -> DateTime -> [Event]
- Overlapping events: overlappingEvents in CalendarTypes.hs
	overlappingEvents :: Calendar -> [(Event, Event)]
- Time in total for events with a given summary: getSpentTimeOn in CalendarTypes.hs
	getSpentTimeOn :: Calendar -> String -> Integer
