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


convertCalendar :: String -> (String, [Error LineColPos])
convertCalendar str = let (cal, err) = execParser parseCalendar str in
	(showCalendar $ foldCalendar antiDuplicate cal, err)


main = do
	-- get command line arguments
	args  <- getArgs
	case args of
		["-o", "stdout", xs] -> do
			contents <- readFile xs
			let (cal, err) = convertCalendar contents
			putStrLn cal
		x : xs -> do
			contents <- readFile x
			let (cal, err) = convertCalendar contents

			putStrLn $ (show (length err)) ++ " parsing errors"
			show_errors err

			putStrLn "Attempting to output result to output.ics"

			writeFile "output.ics" cal

			putStrLn "Output written"
			return ()
