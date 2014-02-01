module EventParse where
import DateParse
import EventTypes
import ParseLib.Abstract

pEvent :: Parser Char Event
pEvent = Event <$
	token "BEGIN:VEVENT"	<* pNewline <*>
	greedy1 pEventProp		<*
	token "END:VEVENT"		<* pNewline

pEventProp :: Parser Char EventProp
pEventProp = DtStamp 	<$ token "DTSTAMP:"		<*> parseDateTime	<* pNewline <|>
	Uid					<$ token "UID:"			<*> pText			<* pNewline <|>
	DtStart				<$ token "DTSTART:"		<*> parseDateTime	<* pNewline <|>
	DtEnd				<$ token "DTEND:"		<*> parseDateTime	<* pNewline <|>
	Description			<$ token "DESCRIPTION:"	<*> pText			<* pNewline <|>
	Summary				<$ token "SUMMARY:"		<*> pText			<* pNewline <|>
	Location			<$ token "LOCATION:"	<*> pText			<* pNewline
