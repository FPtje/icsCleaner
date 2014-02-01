module CalendarParse where
import EventParse
import CalendarTypes
import ParseLib.Abstract
import ParserExtend

parseCalendar :: Parser Char Calendar
parseCalendar =	Calendar <$ token "BEGIN:VCALENDAR"		<*	pNewline	<*>
				greedy1 pCalprop 						<*>
				greedy pEvent <*
				token "END:VCALENDAR" <* pNewline

pCalprop :: Parser Char Calprop
pCalprop = 	Version	<$ token "VERSION:2.0"	<*	pNewline 	<|>
			Prodid	<$ token "PRODID:"		<*> pText 		<* pNewline
