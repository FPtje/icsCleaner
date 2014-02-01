{-# LANGUAGE  FlexibleInstances,
              TypeSynonymInstances,
              MultiParamTypeClasses,
              Rank2Types, FlexibleContexts, NoMonomorphismRestriction,
              CPP  #-}

module DateParse (parseDateTime) where
import Text.ParserCombinators.UU
import Text.ParserCombinators.UU.Utils
import Text.ParserCombinators.UU.BasicInstances
import DateTypes

-- DateTime
pDateTime :: Parser DateTime
pDateTime = DateTime <$> pDate <* pDateSep <*> pTime
parseDateTime = pDateTime -- Alias

-- Date separator
pDateSep :: Parser Char
pDateSep = pSym 'T'

-- Date parsing
pDate :: Parser Date
pDate = Date <$> pQuadDigit <*> pDoubleDigit <*> pDoubleDigit

-- Time parsing
pTime :: Parser Time
pTime = Time <$> pDoubleDigit <*> pDoubleDigit <*> pDoubleDigit <*> pUtcTime

pQuadDigit :: Parser Int
pQuadDigit = (\n1 n2 n3 n4 -> n1 * 1000 + n2 * 100 + n3 * 10 + n4) <$> pDigitAsNum <*> pDigitAsNum <*> pDigitAsNum <*> pDigitAsNum

-- Parsing months, days, hours, minutes and seconds
pDoubleDigit :: Parser Int
pDoubleDigit = (\l r -> l * 10 + r) <$> pDigitAsNum <*> pDigitAsNum

-- UTC Time parsing
pUtcTime :: Parser TimeUTC
pUtcTime = UTC <$ pSym 'Z' `opt` TimeLocal

