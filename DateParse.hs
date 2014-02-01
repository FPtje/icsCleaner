module DateParse (parseDateTime) where
import ParseLib.Abstract
import DateTypes

-- DateTime
pDateTime :: Parser Char DateTime
pDateTime = DateTime <$> pDate <* pDateSep <*> pTime
parseDateTime = pDateTime -- Alias

-- Date separator
pDateSep :: Parser Char Char
pDateSep = symbol 'T'

-- Date parsing
pDate :: Parser Char Date
pDate = Date <$> pYear <*> pDoubleDigit <*> pDoubleDigit

-- Time parsing
pTime :: Parser Char Time
pTime = Time <$> pDoubleDigit <*> pDoubleDigit <*> pDoubleDigit <*> pUtcTime

-- Year parsing
pYear :: Parser Char Year
pYear = Year <$> pDigit <*> pDigit <*> pDigit <*> pDigit

-- Parsing months, days, hours, minutes and seconds
pDoubleDigit :: Parser Char DoubleDigit
pDoubleDigit = DoubleDigit <$> pDigit <*> pDigit

-- UTC Time parsing
pUtcTime :: Parser Char TimeUTC
pUtcTime = UTC <$ symbol 'Z' <<|> succeed TimeLocal

-- Parse a digit Char to a Digit datastructure instance
pDigit :: Parser Char Digit
pDigit = chrToDigit <$> digit
