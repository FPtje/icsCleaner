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

	putStrLn $ (show (length err)) ++ " parsing errors"
	show_errors err

	putStrLn "Attempting to output result to output.ics"

	let noDup = foldCalendar antiDuplicate cal
	writeFile "output.ics" (showCalendar noDup)

	putStrLn "Output written"
	return ()
