{-|
Module      : Byte Count Reader
Description : Read strings like "2kb" and "12 MiB" as counts of bytes
Copyright   : (c) Daniel Rolls, 2020
License     : GPL-3
Maintainer  : daniel.rolls.27@googlemail.com

This library is for reading strings describing a number of bytes like 2Kb and 0.5 MiB.
The units KB, MB, GB and TB imply base 10 (e.g. 2KB = 2 x 1000). The units KiB, MiB, GiB and TiB imply base 2 (e.g. 2KiB = 2 * 1024).
-}
module Data.ByteCountReader (sizeInBytes) where

import Data.Char (toLower)
import Text.ParserCombinators.Parsec (GenParser, many, many1, oneOf, char, parse, anyChar)
import Data.Either.Extra (eitherToMaybe)
import Text.Parsec.Number (floating3)
import GHC.Float.RealFracMethods (roundDoubleInteger)

-- |Read strings describing a number of bytes like 2KB and 0.5 MiB.
-- The units KB, MB, GB and TB are assumed to be base 10 (e.g. 2KB = 2 x 1000).
-- The units KiB, MiB, GiB and TiB are assumed to be base 2 (e.g. 2KiB = 2 * 1024).
sizeInBytes :: String -> Maybe Integer
sizeInBytes inStr = do (number, units) <- eitherToMaybe $ parse bytesParser "<>" inStr
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