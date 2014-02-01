{-# LANGUAGE  FlexibleInstances,
              TypeSynonymInstances,
              MultiParamTypeClasses,
              Rank2Types, FlexibleContexts, NoMonomorphismRestriction,
              CPP  #-}

module EventParse where
import Text.ParserCombinators.UU
import Text.ParserCombinators.UU.Utils
import Text.ParserCombinators.UU.BasicInstances
import Text.ParserCombinators.UU.Derived

import EventTypes
import DateParse

pText :: Parser String
pText = pMunch notNewline
	where
		notNewline c = c /= '\r' && c /= '\n'

pNewline :: Parser String
pNewline = (\l r -> [l, r]) <$> pCR <*> pLF <<|>
	(:[]) <$> (pCR <|> pLF)

pEvent :: Parser Event
pEvent = Event <$
	pToken "BEGIN:VEVENT"	<* pNewline <*>
	pSome  pEventProp		<*
	pToken "END:VEVENT"		<* pNewline

pEventProp :: Parser EventProp
pEventProp = DtStamp 	<$ pToken "DTSTAMP:"		<*> parseDateTime	<* pNewline <|>
	Uid					<$ pToken "UID:"			<*> pText			<* pNewline <|>
	DtStart				<$ pToken "DTSTART:"		<*> parseDateTime	<* pNewline <|>
	DtEnd				<$ pToken "DTEND:"			<*> parseDateTime	<* pNewline <|>
	Description			<$ pToken "DESCRIPTION:"	<*> pText			<* pNewline <|>
	Summary				<$ pToken "SUMMARY:"		<*> pText			<* pNewline <|>
	Location			<$ pToken "LOCATION:"		<*> pText			<* pNewline <|>
	Organizer			<$ pToken "ORGANIZER:"		<*> pText			<* pNewline
