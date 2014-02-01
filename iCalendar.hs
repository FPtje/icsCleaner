module Main where
import System.IO
import Data.Maybe
import System.Environment
import System.FilePath

import CalendarTypes
import CalendarParse
import DateTypes
import EventTypes
import AntiDuplicate
import PrintCalendar
import Text.ParserCombinators.UU.Utils
import Text.ParserCombinators.UU.BasicInstances


-- Test:
testString = "BEGIN:VCALENDAR\r\nVERSION:2.0\r\nPRODID:-//hacksw/handcal//NONSGML v1.0//EN\r\nBEGIN:VEVENT\r\nUID:19970610T172345Z-AF23B2@example.com\r\nDTSTAMP:19970610T172345Z\r\nDTSTART:19970714T170000Z\r\nDTEND:19970715T040000Z\r\nSUMMARY:Bastille Day Party\r\nEND:VEVENT\r\nBEGIN:VEVENT\r\nUID:19970610T172345Z-AF23B3@example.com\r\nDTSTAMP:19990610T172345Z\r\nDTSTART:19940714T170000Z\r\nDTEND:19970715T040000Z\r\nSUMMARY:Bastille Day Party\r\nEND:VEVENT\r\nBEGIN:VEVENT\r\nUID:19970610T172345Z-AF23B2@example.com\r\nDTSTAMP:19970610T172345Z\r\nDTSTART:19970714T170000Z\r\nDTEND:19970715T040000Z\r\nSUMMARY:Bastille Day Party\r\nEND:VEVENT\r\nBEGIN:VEVENT\r\nUID:19970610T172345Z-AF23B3@example.com\r\nDTSTAMP:19990610T172345Z\r\nDTSTART:19940714T170000Z\r\nDTEND:19970715T040000Z\r\nSUMMARY:Bastille Day Party\r\nEND:VEVENT\r\nEND:VCALENDAR\r\n"


testCalendar = execParser parseCalendar testString
testDateTime  = DateTime (Date 1997 07 04) (Time 18 04 59 UTC)

testDateTime1 = DateTime (Date 2002 07 04) (Time 18 06 00 UTC)

testDateTime2 = DateTime (Date 2000 07 04) (Time 18 04 59 UTC)

testDateTime3 = DateTime (Date 2001 07 04) (Time 18 04 59 UTC)

testDateTime4 = DateTime (Date 2001 07 04) (Time 18 04 59 UTC)

testDateTime5 = DateTime (Date 2001 07 04) (Time 19 04 59 UTC)

testEvent1 = Event [DtStart testDateTime, DtEnd testDateTime1]

testEvent2 = Event [DtStart testDateTime2, DtEnd testDateTime3]


{-
	IO
-}
readCalendar :: FilePath -> IO (Calendar, [Error LineColPos])
readCalendar path = do
	handle 		<- openFile path ReadMode
	hSetNewlineMode handle noNewlineTranslation

	contents 	<- hGetContents handle
	putStr ""
	let res = execParser parseCalendar contents
	return res

show_errors = sequence_ . (map (putStrLn . show))

main = do
	-- get command line arguments
	args  <- getArgs
	contents <- readFile (head args)
	let (cal, err) = execParser parseCalendar contents

	putStrLn "Parse errors:"
	show_errors err

	let noDup = foldCalendar antiDuplicate cal
	writeFile "output.ics" (showCalendar noDup)
