module Main where
import CalendarParse
import ParserExtend
import System.IO
import Data.Maybe

import CalendarTypes
import DateTypes
import EventTypes
import Printing


-- Test:
testString = "BEGIN:VCALENDAR\r\nVERSION:2.0\r\nPRODID:-//hacksw/handcal//NONSGML v1.0//EN\r\nBEGIN:VEVENT\r\nUID:19970610T172345Z-AF23B2@example.com\r\nDTSTAMP:19970610T172345Z\r\nDTSTART:19970714T170000Z\r\nDTEND:19970715T040000Z\r\nSUMMARY:Bastille Day Party\r\nEND:VEVENT\r\nBEGIN:VEVENT\r\nUID:19970610T172345Z-AF23B3@example.com\r\nDTSTAMP:19990610T172345Z\r\nDTSTART:19940714T170000Z\r\nDTEND:19970715T040000Z\r\nSUMMARY:Bastille Day Party\r\nEND:VEVENT\r\nBEGIN:VEVENT\r\nUID:19970610T172345Z-AF23B2@example.com\r\nDTSTAMP:19970610T172345Z\r\nDTSTART:19970714T170000Z\r\nDTEND:19970715T040000Z\r\nSUMMARY:Bastille Day Party\r\nEND:VEVENT\r\nBEGIN:VEVENT\r\nUID:19970610T172345Z-AF23B3@example.com\r\nDTSTAMP:19990610T172345Z\r\nDTSTART:19940714T170000Z\r\nDTEND:19970715T040000Z\r\nSUMMARY:Bastille Day Party\r\nEND:VEVENT\r\nEND:VCALENDAR\r\n"


testCalendar = fromJust $ run parseCalendar testString
testDateTime = DateTime (Date (Year One Nine Nine Seven) (DoubleDigit Zero Seven) (DoubleDigit One Four)) (Time (DoubleDigit One Eight) (DoubleDigit Zero Four) (DoubleDigit Five Nine) UTC)

testDateTime1 = DateTime (Date (Year Two Zero Zero Two) (DoubleDigit Zero Seven) (DoubleDigit One Four)) (Time (DoubleDigit One Eight) (DoubleDigit One Five) (DoubleDigit Zero Zero) UTC)

testDateTime2 = DateTime (Date (Year Two Zero Zero Zero) (DoubleDigit Zero Seven) (DoubleDigit One Four)) (Time (DoubleDigit One Eight) (DoubleDigit Zero Four) (DoubleDigit Five Nine) UTC)

testDateTime3 = DateTime (Date (Year Two Zero Zero One) (DoubleDigit Zero Seven) (DoubleDigit One Four)) (Time (DoubleDigit One Eight) (DoubleDigit Zero Four) (DoubleDigit Five Nine) UTC)

testDateTime4 = DateTime (Date (Year Two Zero Zero One) (DoubleDigit Zero Seven) (DoubleDigit One Four)) (Time (DoubleDigit One Eight) (DoubleDigit Zero Four) (DoubleDigit Five Nine) UTC)

testDateTime5 = DateTime (Date (Year Two Zero Zero One) (DoubleDigit Zero Seven) (DoubleDigit One Four)) (Time (DoubleDigit One Nine) (DoubleDigit Zero Four) (DoubleDigit Five Nine) UTC)

testEvent1 = Event [DtStart testDateTime, DtEnd testDateTime1]

testEvent2 = Event [DtStart testDateTime2, DtEnd testDateTime3]


{-
	IO
-}
readCalendar :: FilePath -> IO (Maybe Calendar)
readCalendar path = do
	handle 		<- openFile path ReadMode
	hSetNewlineMode handle noNewlineTranslation

	contents 	<- hGetContents handle
	return (run parseCalendar contents)

main = putStr ""
