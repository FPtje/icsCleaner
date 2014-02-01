module ParserExtend where
import ParseLib.Abstract
import DateParse
import DateTypes

pNewline :: Parser Char String
pNewline = token "\r\n"

pText :: Parser Char String
pText = greedy1 (satisfy notNewline)
	where
		notNewline c = c /= '\r' && c /= '\n'

-- Parse everything or nothing
pFullParse :: Parser a b -> Parser a b
pFullParse p = p <* eof

-- Run a parser, parse everything or nothing and give the first result
run :: Parser a b -> [a] -> Maybe b
run p str = let parsed = parse (pFullParse p) str in
	if null parsed then Nothing
	else Just $ (fst . head) parsed

parsePrint :: String -> Maybe String
parsePrint s = printDateTime <$> run parseDateTime s
