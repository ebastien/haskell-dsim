{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Types (
      paddedDecimalP
    , decimalP
    , AirlineCode
    , airlineP
    , toAirlineCode
    , Port
    , portP
    , toPort
    , OnD
    , Path
    , PeriodBoundary
    , dateP
    , toDate
    , periodBoundaryP
    , Dow
    , dowP
    , Period
    , withinPeriod
    , TimeDuration
    , TimeVariation
    , timeVariationP
    , dateVariationP
    , ScheduleTime
    , scheduleTimeP
    , fnumP
    , pointsIndicatorP
    , segmentIdx
    ) where

import qualified Data.ByteString.Char8 as B8

import Data.ByteString.Lex.Integral (readDecimal)

import Data.Word (Word8)
import Data.Bits (bit, testBit)
import Data.Char (chr, ord)

import Data.Time.Clock (DiffTime, secondsToDiffTime)
import Data.Time.Calendar (Day, fromGregorianValid)
import Data.Time.LocalTime (makeTimeOfDayValid, timeOfDayToTime)
import Data.Time.Calendar.WeekDate (toWeekDate)

import Data.List (elemIndex)
import Data.Maybe (fromMaybe)
import Data.Functor ((<$>))
import Control.Applicative (pure, (<*>), (<*), (*>), (<|>))

import qualified Data.Attoparsec.ByteString.Char8 as P
import Data.Attoparsec.ByteString (Parser, (<?>))

-- | Parser for fixed length decimal numbers
-- with space padding and defaulting to zero.
paddedDecimalP :: Int -> Parser Int
paddedDecimalP n = do
  s <- B8.dropWhile (== ' ') <$> P.take n
  fromMaybe (fail ("Decimal parsing failed on " ++ show s))
          $ (return . fst) <$> if B8.null s
                                 then Just (0, B8.empty)
                                 else readDecimal s

-- | Parser for fixed length decimal numbers.
decimalP :: Int -> Parser Int
decimalP n = do
  i <- readDecimal <$> P.take n
  fromMaybe (fail "Decimal parsing failed") $ (return . fst) <$> i

-- | Parser generator for values packed as numbers.
packWith :: Num a => Int -> (Int -> Parser a) -> Parser a
packWith n f | n > 0     = sum <$> (sequence $ map f [0..n-1])
             | otherwise = error "Invalid packing length"

-- | ByteString parsing to Maybe.
maybeParse :: Parser a -> B8.ByteString -> Maybe a
maybeParse p = either (const Nothing) Just . P.parseOnly p

{-------------------------------------------------------------------------------
  Airline code
-------------------------------------------------------------------------------}

newtype AirlineCode = MkAirlineCode Int deriving (Eq, Ord)

instance Show AirlineCode where
  show (MkAirlineCode a) = loop (3 :: Int) a
    where loop n i | n == 0    = []
                   | otherwise = c : loop (n-1) q
            where (q,r) = divMod i 37
                  c | r == 0    = ' '
                    | r < 11    = chr (r -  1 + ord '0')
                    | otherwise = chr (r - 11 + ord 'A')

-- | Parser for airline codes.
airlineP :: Parser AirlineCode
airlineP = MkAirlineCode <$> packWith 3 step <?> "Airline code"
  where step n = (* 37^n) <$> code n
        code n | n < 2     = letter <|> digit
               | otherwise = letter <|> digit <|> space
        letter = (+11) . subtract (ord 'A') . ord <$> P.satisfy isUpperLetter
        digit  =  (+1) . subtract (ord '0') . ord <$> P.digit
        space  = pure 0 <* P.space
        isUpperLetter c = c >= 'A' && c <= 'Z'

-- | Try to convert a ByteString to an AirlineCode.
toAirlineCode :: B8.ByteString -> Maybe AirlineCode
toAirlineCode = maybeParse airlineP

{-------------------------------------------------------------------------------
  Port
-------------------------------------------------------------------------------}

newtype Port = MkPort Int deriving (Eq, Ord, Enum)

instance Show Port where
  show (MkPort p) = loop (3 :: Int) p
    where loop n i | n == 0    = []
                   | otherwise = chr (r + ord 'A') : loop (n-1) q
            where (q,r) = divMod i 26

-- | Parser for ports.
portP :: Parser Port
portP = MkPort <$> packWith 3 step <?> "Port"
  where step n = (* 26^n) . subtract (ord 'A') . ord <$> P.satisfy letter
        letter c = c >= 'A' && c <= 'Z'

-- | Try to convert a ByteString to a Port.
toPort :: B8.ByteString -> Maybe Port
toPort = maybeParse portP

-- | Origin & Destination.
type OnD = (Port, Port)

-- | A sequence of ports.
type Path = [Port]

{-------------------------------------------------------------------------------
  Days of the week
-------------------------------------------------------------------------------}

newtype Dow = MkDow Word8

instance Show Dow where
  show (MkDow w) = map step [0..6]
    where step n | testBit w n = chr (n + ord '1')
                 | otherwise   = ' '

-- | Parser for days of week.
dowP :: Parser Dow
dowP = MkDow <$> packWith 7 step <?>  "Days of week"
  where step n = P.char (chr $ ord '1' + n) *> (pure $ bit n)
             <|> P.char ' ' *> (pure 0)

-- | Try to convert a ByteString to days of the week.
toDow :: B8.ByteString -> Maybe Dow
toDow = maybeParse dowP

-- | Lookup a single day of week.
lookupDow :: Int -> Dow -> Bool
lookupDow n (MkDow w) = testBit w (n-1)

{-------------------------------------------------------------------------------
  Period
-------------------------------------------------------------------------------}

type PeriodBoundary = Maybe Day

type Period = (Day, PeriodBoundary, Dow)

-- | Parser for days.
dayP :: Parser Int
dayP = decimalP 2

-- | Parser for months.
monthP :: Parser Int
monthP = do
  m <- (flip elemIndex) months <$> P.take 3
  fromMaybe (fail "Month parsing failed") $ return <$> m
  where months = [ "XXX", "JAN","FEB", "MAR","APR", "MAY",
                   "JUN","JUL","AUG", "SEP", "OCT", "NOV", "DEC" ]

-- | Parser for years.
yearP :: Num a => Parser a
yearP = (fromIntegral . (2000+)) <$> decimalP 2

-- | Parse for dates.
dateP :: Parser Day
dateP = do
  d <- dayP; m <- monthP; y <- yearP
  fromMaybe (fail "Date parsing failed")
          $ return <$> (fromGregorianValid y m d)

-- | Try to convert a ByteString to a date.
toDate :: B8.ByteString -> Maybe Day
toDate = maybeParse dateP

-- | Parser for period boundaries.
periodBoundaryP :: Parser PeriodBoundary
periodBoundaryP = do
  d <- dayP; m <- monthP; y <- yearP
  if d == 0 && m == 0
    then return Nothing
    else fromMaybe (fail "Period boundary parsing failed")
                 $ (return . Just) <$> (fromGregorianValid y m d)

-- | Try to convert a ByteString to a period boundary.
toPeriodBoundary :: B8.ByteString -> Maybe PeriodBoundary
toPeriodBoundary = maybeParse periodBoundaryP

-- | Test if a day is within a period.
withinPeriod :: Period -> Day -> Bool
withinPeriod (l,h,o) d = low && high && dow
  where low = d >= l
        high = case h of
                 Just h' -> d <= h'
                 Nothing -> True
        dow = let (_, _, n) = toWeekDate d in lookupDow n o

{-------------------------------------------------------------------------------
  Date and time
-------------------------------------------------------------------------------}

type TimeDuration = DiffTime
type TimeVariation = DiffTime
type ScheduleTime = DiffTime

-- | Parser for time variations.
timeVariationP :: Parser TimeVariation
timeVariationP = (plus <|> minus) <*> time
  where plus = P.char '+' *> pure id
        minus = P.char '-' *> pure negate
        time = do
          h <- decimalP 2; m <- decimalP 2
          if h <= 23 && m <= 59
            then return . secondsToDiffTime . fromIntegral $ 60 * h + m
            else fail "Time variation parsing failed"

-- | Parser for date variations.
dateVariationP :: Parser Int
dateVariationP = fromIntegral <$> (before <|> after)
  where before = pure (-1) <* P.char 'J'
        after  = subtract (ord '0') . ord <$> P.digit

-- | Parser for schedule times.
scheduleTimeP :: Parser ScheduleTime
scheduleTimeP = do
  m <- makeTimeOfDayValid <$> decimalP 2 <*> decimalP 2 <*> pure 0
  fromMaybe (fail "Local time parsing failed") $ return . timeOfDayToTime <$> m

{-------------------------------------------------------------------------------
  Other parsers
-------------------------------------------------------------------------------}

-- | Parser for flight numbers.
fnumP :: Parser Int
fnumP = paddedDecimalP 4

-- | Parser for board and off points indicators.
pointsIndicatorP :: Parser Int
pointsIndicatorP = packWith 2 step <?> "Board and off points indicator"
  where step n = (* 26^n) . subtract (ord 'A') . ord <$> P.satisfy letter
        letter c = c >= 'A' && c <= 'Z'

-- | Segment index from leg sequences
segmentIdx :: Int -> Int -> Int
segmentIdx board off = off * 26 + board - 1
