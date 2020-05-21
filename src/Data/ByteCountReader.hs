module Data.ByteCountReader (getSize) where

import Data.Char (toLower)
import Text.ParserCombinators.Parsec (GenParser, many, many1, oneOf, char, parse, anyChar)
import Data.Either.Extra (eitherToMaybe)
import Text.Parsec.Number (floating3)
import GHC.Float.RealFracMethods (roundDoubleInteger)

getSize :: String -> Maybe Integer
getSize inStr = do (number, units) <- eitherToMaybe $ parse bytesParser "<>" inStr
                   roundDoubleInteger . (number *) . fromInteger <$> toMultiplier units

bytesParser :: GenParser Char st (Double, String)
bytesParser = do num <- parseNumber
                 parseSpaces
                 units <- parseUnits
                 return (num, units)
               where parseSpaces = many $ char ' '
                     parseNumber = floating3 False
                     parseUnits = many1 anyChar

toMultiplier :: String -> Maybe Integer
toMultiplier = mapUnits . map toLower
               where mapUnits "b"   = _1024RaisedTo  0
                     mapUnits "kb"  = _1000RaisedTo  1
                     mapUnits "kib" = _1024RaisedTo  1
                     mapUnits "mb"  = _1000RaisedTo  2
                     mapUnits "mib" = _1024RaisedTo  2
                     mapUnits "gb"  = _1000RaisedTo  3
                     mapUnits "gib" = _1024RaisedTo  3
                     mapUnits "tb"  = _1000RaisedTo  4
                     mapUnits "tib" = _1024RaisedTo  4
                     mapUnits _     = Nothing
                     _1024RaisedTo = Just . (1024 ^)
                     _1000RaisedTo = Just . (1000 ^)

