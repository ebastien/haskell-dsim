{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Ssim
    ( Port
    , toPort
    ) where

import Criterion.Main

import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy as LB

import Data.Attoparsec.ByteString (Parser, (<?>))
import qualified Data.Attoparsec.ByteString.Char8 as P
import qualified Data.Attoparsec.ByteString.Lazy as LP

import Data.Functor ((<$>))
import Control.Monad (void)
import Control.Applicative (pure, some, (<*>), (<*), (*>), (<|>))

import Data.Maybe (fromMaybe)

import Data.Time.LocalTime (TimeOfDay, makeTimeOfDayValid, timeOfDayToTime)
import Data.Time.Calendar (Day, fromGregorianValid)
import Data.Time.Clock (DiffTime, secondsToDiffTime)

import Data.List (elemIndex)

import Data.Word (Word8)
import Data.Bits (bit, testBit)
import Data.Char (chr, ord)

import Data.ByteString.Lex.Integral (readDecimal)

import Data.Ix (Ix)

newtype Dow = MkDow Word8

instance Show Dow where
  show (MkDow w) = map (step w) [0..6]
    where step w n | testBit w n = chr (n + ord '1')
                   | otherwise       = ' '

newtype AirlineCode = MkAirlineCode Int deriving (Eq, Ord, Ix)

instance Show AirlineCode where
  show (MkAirlineCode a) = loop 3 a
    where loop n i | n == 0    = []
                   | otherwise = c : loop (n-1) q
            where (q,r) = divMod i 37
                  c | r == 0    = ' '
                    | r < 11    = chr (r -  1 + ord '0')
                    | otherwise = chr (r - 11 + ord 'A')

newtype Port = MkPort Int deriving (Eq, Ord, Ix, Enum)

instance Show Port where
  show (MkPort p) = loop 3 p
    where loop n i | n == 0    = []
                   | otherwise = chr (r + ord 'A') : loop (n-1) q
            where (q,r) = divMod i 26

type PeriodBoundary = Maybe Day
type TimeDuration = DiffTime
type TimeVariation = DiffTime
type DateVariation = DiffTime
type ScheduleTime = DiffTime

data Header = Header deriving Show

data Carrier = Carrier { cAirline :: !AirlineCode 
                       } deriving Show

data Flight = Flight { fAirline :: !AirlineCode
                     , fNumber :: !Int
                     , fSuffix :: !Char
                     } deriving Show

data Period = Period { pBegin :: !PeriodBoundary
                     , pEnd :: !PeriodBoundary
                     , pDow :: !Dow
                     } deriving Show

data Leg = Leg { lSequence :: !Int
               , lBoard :: !Port
               , lOff :: !Port
               , lDepartureTime :: !ScheduleTime
               , lArrivalTime :: !ScheduleTime
               , lElapsedTime :: !TimeDuration
               } deriving Show

data LegPeriod = LegPeriod { lpFlight :: Flight
                           , lpVariation :: !Int
                           , lpPeriod :: Period
                           , lpLeg :: Leg
                           } deriving Show

data Segment = Segment { sIndex :: !Int
                       , sBoard :: !Port
                       , sOff :: !Port
                       , sDEI :: !Int
                       } deriving Show

data SegmentData = SegmentData { sdFlight :: Flight
                               , sdVariation :: Int
                               , sdSegment :: Segment
                               } deriving Show

data LegGroup = LegGroup { lgLeg :: LegPeriod
                         , lgSegments :: [SegmentData] } deriving Show

data CarrierGroup = CarrierGroup { cgCarrier :: Carrier
                                 , cgLegs :: [LegGroup] } deriving Show

data Ssim = Ssim { ssimHeader :: Header
                 , ssimCarriers :: [CarrierGroup] } deriving Show

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

-- | Parser for period boundaries.
periodBoundaryP :: Parser PeriodBoundary
periodBoundaryP = do
  d <- dayP; m <- monthP; y <- yearP
  if d == 0 && m == 0
    then return Nothing
    else fromMaybe (fail "Period boundary parsing failed")
                 $ (return . Just) <$> (fromGregorianValid y m d)

-- | Parser for flight numbers.
fnumP :: Parser Int
fnumP = paddedDecimalP 4

-- | Parser generator for values packed as numbers.
packWith :: Num a => Int -> (Int -> Parser a) -> Parser a
packWith n f | n > 0 = sum <$> (sequence $ map f [0..n-1])

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

-- | Parser for days of week.
dowP :: Parser Dow
dowP = MkDow <$> packWith 7 step <?>  "Days of week"
  where step n = P.char (chr $ ord '1' + n) *> (pure $ bit n)
             <|> P.char ' ' *> (pure 0)

-- | Parser for ports.
portP :: Parser Port
portP = MkPort <$> packWith 3 step <?> "Port"
  where step n = (* 26^n) . subtract (ord 'A') . ord <$> P.satisfy letter
        letter c = c >= 'A' && c <= 'Z'

-- | ByteString parsing to Maybe.
maybeParse :: Parser a -> B8.ByteString -> Maybe a
maybeParse p = either (const Nothing) Just . P.parseOnly p

-- | Try to convert a ByteString to a Port.
toPort :: B8.ByteString -> Maybe Port
toPort = maybeParse portP

-- | Try to convert a ByteString to an AirlineCode.
toAirlineCode :: B8.ByteString -> Maybe AirlineCode
toAirlineCode = maybeParse airlineP

-- | Parser for schedule times.
scheduleTimeP :: Parser ScheduleTime
scheduleTimeP = do
  m <- makeTimeOfDayValid <$> decimalP 2 <*> decimalP 2 <*> pure 0
  fromMaybe (fail "Local time parsing failed") $ return . timeOfDayToTime <$> m

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
dateVariationP :: Parser DateVariation
dateVariationP = secondsToDiffTime . (*86400) . fromIntegral
                 <$> (before <|> after)
  where before = pure (-1) <* P.char 'A'
        after  = subtract (ord '0') . ord <$> P.digit

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
  bdate <- periodBoundaryP  <?> "Leg period of operation (from)"
  edate <- periodBoundaryP  <?> "Leg period of operation (to)"
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
      flight = Flight airline fnum suffix
      period = Period bdate edate dow
      etime = (atime - atvar + advar) - (dtime - dtvar + ddvar)
      leg = Leg lsn bpoint opoint dtime atime etime
  return $ LegPeriod flight variation period leg

-- | Parser for segment records.
segmentP :: Parser SegmentData
segmentP = do
  void (P.char '4')         <?> "Segment record type"
  suffix <- P.anyChar       <?> "Segment operational suffix"
  airline <- airlineP       <?> "Segment airline code"
  fnum <- fnumP             <?> "Segment flight number"
  iviL <- decimalP 2        <?> "Segment itinerary variation identifier (low)"
  lsn <- decimalP 2         <?> "Segment leg sequence number"
  void $ P.anyChar
  void $ P.take 13
  iviH <- paddedDecimalP 1  <?> "Segment itinerary variation identifier (high)"
  void $ P.anyChar
  void $ P.anyChar
  dei <- decimalP 3         <?> "Segment DEI"
  bpoint <- portP           <?> "Segment board point"
  opoint <- portP           <?> "Segment off point"
  void $ P.take 155
  void $ P.take 6
  void $ some P.endOfLine
  let variation = iviL + 100 * iviH
      flight = Flight airline fnum suffix
      idx = undefined
      segment = Segment idx bpoint opoint dei
  return $ SegmentData flight variation segment

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
carrierGroupP = CarrierGroup <$> (carrierP       <?> "Carrier record")
                             <*> (some legGroupP <?> "Carrier legs")

-- | Parser for SSIM file.
ssimP :: Parser Ssim
ssimP = Ssim <$> (headerP            <?> "SSIM7 header")
             <*> (some carrierGroupP <?> "SSIM7 carriers")
             <*  (trailerP           <?> "SSIM7 trailer")

-- | Run the SSIM parser on the given file.
parseSsimFile :: String -> IO (LP.Result Ssim)
parseSsimFile s = LP.parse ssimP <$> LB.readFile s

main :: IO ()
main = defaultMain [
  bench "lazy" $ whnfIO $ parseSsimFile input
  ]
  where input = "oag.ssim7.sample"
