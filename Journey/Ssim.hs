{-# LANGUAGE OverloadedStrings #-}

module Ssim (
      readSsimFile
    , ssimSegments
    , toPort
    , toDate
    ) where

import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy as LB
import Data.ByteString.Lex.Integral (readDecimal)
import Data.Attoparsec.ByteString (Parser, (<?>))
import qualified Data.Attoparsec.ByteString.Char8 as P
import qualified Data.Attoparsec.ByteString.Lazy as LP

import Data.Functor ((<$>))
import Control.Monad (void, join)
import Control.Applicative (pure, some, (<*>), (<*), (*>), (<|>))
import Data.List (groupBy, elemIndex)
import Data.Function (on)
import Data.Maybe (fromMaybe)
import Data.Char (ord, chr)
import Data.Bits (bit)

import Data.Time.Clock (secondsToDiffTime)
import Data.Time.Calendar (Day, fromGregorianValid)
import Data.Time.LocalTime (timeOfDayToTime, makeTimeOfDayValid)

import Types ( AirlineCode(..), LegPeriod(..), SegmentPeriod
             , SegmentData(..), Flight(..), Port(..), Dow(..)
             , PeriodBoundary, TimeVariation, ScheduleTime, OnD, segmentIdx)

{-------------------------------------------------------------------------------
  SSIM data types
-------------------------------------------------------------------------------}

data Header = Header deriving Show

data Carrier = Carrier { cAirline :: !AirlineCode 
                       } deriving Show

data LegGroup = LegGroup { lgLeg :: LegPeriod
                         , lgSegments :: [SegmentData] } deriving Show

type FlightGroup = [LegGroup]

data CarrierGroup = CarrierGroup { cgCarrier :: Carrier
                                 , cgFlights :: [FlightGroup] } deriving Show

data Ssim = Ssim { ssimHeader :: Header
                 , ssimCarriers :: [CarrierGroup] } deriving Show

{-------------------------------------------------------------------------------
  Parsers
-------------------------------------------------------------------------------}

-- | Parser for header records.
headerP :: Parser Header
headerP  = P.char '1' >> P.take 199 >> (some P.endOfLine) >> return Header

-- | Parser for carrier records.
carrierP :: Parser Carrier
carrierP = do
  void $ P.char '2'
  void $ P.satisfy (\c -> c == 'U' || c == 'L')
  airline <- airlineP
  void $ P.take 195
  void $ some P.endOfLine
  return (Carrier airline)

-- | Parser for leg records.
legPeriodP :: Parser LegPeriod
legPeriodP = do
  void (P.char '3')         <?> "Leg record type"
  suffix <- P.anyChar
  airline <- airlineP
  fnum <- fnumP             <?> "Leg flight number"
  iviL <- decimalP 2        <?> "Leg itinerary variation identifier (low)"
  lsn <- decimalP 2         <?> "Leg sequence number"
  void $ P.anyChar
  bdate <- dateP            <?> "Leg period of operation (begin)"
  edate <- periodBoundaryP  <?> "Leg period of operation (end)"
  dow <- dowP               <?> "Leg days of week"
  void $ P.anyChar
  bpoint <- portP           <?> "Leg board point"
  void $ P.take 4
  dtime <- scheduleTimeP    <?> "Leg departure time"
  dtvar <- timeVariationP   <?> "Leg departure time variation"
  void $ P.take 2
  opoint <- portP           <?> "Leg off point"
  atime <- scheduleTimeP    <?> "Leg arrival time"
  void $ P.take 4
  atvar <- timeVariationP   <?> "Leg arrival time variation"
  void $ P.take (2+3+20+5+10+9+2+6)
  iviH <- paddedDecimalP 1  <?> "Leg itinerary variation identifier (high)"
  void $ P.take (3+3+3+9+1+1+1+11+1+11+20)
  ddvar <- dateVariationP   <?> "Leg departure date variation"
  advar <- dateVariationP   <?> "Leg arrival date variation"
  void $ P.take 6
  void $ some P.endOfLine
  let variation = iviL + 100 * iviH
      flight = Flight airline fnum suffix variation
      period = (bdate, edate, dow)
      etime = (atime - atvar) - (dtime - dtvar)
            + (secondsToDiffTime . fromIntegral $ (advar - ddvar) * 86400)
  return $ LegPeriod flight period lsn
                     bpoint opoint
                     dtime atime etime advar

-- | Parser for segment records.
segmentP :: Parser SegmentData
segmentP = do
  void (P.char '4')         <?> "Segment record type"
  suffix <- P.anyChar       <?> "Segment operational suffix"
  airline <- airlineP       <?> "Segment airline code"
  fnum <- fnumP             <?> "Segment flight number"
  iviL <- decimalP 2        <?> "Segment itinerary variation identifier (low)"
  _lsn <- decimalP 2        <?> "Segment leg sequence number"
  void $ P.anyChar
  void $ P.take 13
  iviH <- paddedDecimalP 1  <?> "Segment itinerary variation identifier (high)"
  idx <- pointsIndicatorP   <?> "Segment points indicator"
  dei <- decimalP 3         <?> "Segment DEI"
  bpoint <- portP           <?> "Segment board point"
  opoint <- portP           <?> "Segment off point"
  void $ P.take 155
  void $ P.take 6
  void $ some P.endOfLine
  let variation = iviL + 100 * iviH
      flight = Flight airline fnum suffix variation
  return $ SegmentData flight idx bpoint opoint dei

-- | Parser for trailer records.
trailerP :: Parser ()
trailerP = (P.char '5'       <?> "Trailer record type")
        *> (P.take 199       <?> "Trailer padding")
        *> (some P.endOfLine <?> "Trailer separator")
        *> pure ()

-- | Parser for leg groups.
legGroupP :: Parser LegGroup
legGroupP = LegGroup <$> (legPeriodP    <?> "Leg record")
                     <*> (some segmentP <?> "Leg segments")

-- | Parser for carrier groups.
carrierGroupP :: Parser CarrierGroup
carrierGroupP = CarrierGroup <$> (carrierP               <?> "Carrier record")
                             <*> (grp <$> some legGroupP <?> "Carrier legs")
                             <*  (trailerP               <?> "Carrier trailer")
  where grp = groupBy ((==) `on` lpFlight . lgLeg)

-- | Parser for SSIM file.
ssimP :: Parser Ssim
ssimP = Ssim <$> (headerP            <?> "SSIM7 header")
             <*> (some carrierGroupP <?> "SSIM7 carriers")

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

-- | Parser for ports.
portP :: Parser Port
portP = MkPort <$> packWith 3 step <?> "Port"
  where step n = (* 26^n) . subtract (ord 'A') . ord <$> P.satisfy letter
        letter c = c >= 'A' && c <= 'Z'

-- | Try to convert a ByteString to a Port.
toPort :: B8.ByteString -> Maybe Port
toPort = maybeParse portP

-- | Parser for days of week.
dowP :: Parser Dow
dowP = MkDow <$> packWith 7 step <?>  "Days of week"
  where step n = P.char (chr $ ord '1' + n) *> (pure $ bit n)
             <|> P.char ' ' *> (pure 0)

-- | Try to convert a ByteString to days of the week.
toDow :: B8.ByteString -> Maybe Dow
toDow = maybeParse dowP

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

-- | Parser for flight numbers.
fnumP :: Parser Int
fnumP = paddedDecimalP 4

-- | Parser for board and off points indicators.
pointsIndicatorP :: Parser Int
pointsIndicatorP = packWith 2 step <?> "Board and off points indicator"
  where step n = (* 26^n) . subtract (ord 'A') . ord <$> P.satisfy letter
        letter c = c >= 'A' && c <= 'Z'

{-------------------------------------------------------------------------------
  Interface
-------------------------------------------------------------------------------}

-- | Run the SSIM parser on the given file.
readSsimFile :: String -> IO Ssim
readSsimFile s = do
  result <- LP.parse ssimP <$> LB.readFile s
  case result of
    LP.Fail left ctx msg -> fail . unlines $ msg:(show $ LB.length left):ctx
    LP.Done _ ssim      -> return ssim

-- | Extract segments from a flight
flightSegments :: FlightGroup -> [(OnD, SegmentPeriod)]
flightSegments = join . combine
  where combine []         = []
        combine xs@(x:xs') = [ mkAssoc y | y <- xs ] : combine xs'
          where legX = lgLeg x
                mkAssoc y = ((lpBoard legX, lpOff legY), map select legs)
                  where legY = lgLeg y
                        legs = takeWhile (on (>=) (lpSequence . lgLeg) $ y) xs
                        select l = (legL, map dDEI . filter ((==) idx . dIndex) $ segL)
                          where segL = lgSegments l
                                legL = lgLeg l
                                idx = segmentIdx (lpSequence legL) (lpSequence legY)

-- | Extract segments from a SSIM structure.
ssimSegments :: Ssim -> [(OnD, SegmentPeriod)]
ssimSegments ssim = do
  car <- ssimCarriers ssim
  flt <- cgFlights car
  flightSegments flt
