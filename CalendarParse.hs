{-# LANGUAGE  FlexibleInstances,
              TypeSynonymInstances,
              MultiParamTypeClasses,
              Rank2Types, FlexibleContexts, NoMonomorphismRestriction,
              CPP  #-}

module CalendarParse where
import Text.ParserCombinators.UU
import Text.ParserCombinators.UU.Utils
import Text.ParserCombinators.UU.BasicInstances
import Text.ParserCombinators.UU.Derived

import EventParse
import CalendarTypes

parseCalendar :: Parser Calendar
parseCalendar =	Calendar <$ pToken "BEGIN:VCALENDAR"	<*	pNewline	<*>
				pSome pCalprop 						    <*>
				pMany pEvent            				<*
				pToken "END:VCALENDAR" 					<* pNewline

pCalprop :: Parser Calprop
pCalprop = 	Version	<$ pToken "VERSION:2.0"	<*	pNewline 	<|>
			Prodid	<$ pToken "PRODID:"		<*> pText 		<* pNewline
