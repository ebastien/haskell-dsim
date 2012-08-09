{-# LANGUAGE OverloadedStrings #-}

import qualified Filesystem.Path.CurrentOS as FP

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

import Data.Time.LocalTime (TimeOfDay, makeTimeOfDayValid)
import Data.Time.Calendar (Day, fromGregorianValid)

import Data.List (elemIndex)

import Data.Word (Word8)
import Data.Bits (bit)
import Data.Char (ord)

type PeriodBoundary = Maybe Day
type Dow = Word8
type Port = Int
type AirlineCode = Int

data Header = Header deriving Show

data Carrier = Carrier { cAirline :: AirlineCode 
                       } deriving Show

data Flight = Flight { fAirline :: AirlineCode
                     , fNumber :: Int
                     , fSuffix :: Char
                     } deriving Show

data Period = Period { pBegin :: PeriodBoundary
                     , pEnd :: PeriodBoundary
                     , pDow :: Dow
                     } deriving Show

data Leg = Leg { lSequence :: Int
               , lBoard :: Port
               , lOff :: Port
               } deriving Show

data LegPeriod = LegPeriod { lpFlight :: Flight
                           , lpPeriod :: Period
                           , lpVariation :: Int
                           , lpLeg :: Leg
                           } deriving Show

data Segment = Segment deriving Show

data LegGroup = LegGroup { lgLeg :: LegPeriod
                         , lgSegments :: [Segment] } deriving Show

data CarrierGroup = CarrierGroup { cgCarrier :: Carrier
                                 , cgLegs :: [LegGroup] } deriving Show

data Ssim = Ssim { ssimHeader :: Header
                 , ssimCarriers :: [CarrierGroup] } deriving Show

headerP :: Parser Header
headerP  = P.char '1' >> P.take 199 >> (some P.endOfLine) >> return Header

carrierP :: Parser Carrier
carrierP = do
  void $ P.char '2'
  void $ P.satisfy (\c -> c == 'U' || c == 'L')
  airline <- airlineP
  void $ P.take 195
  void $ some P.endOfLine
  return (Carrier airline)

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
  dtime <- P.take 4
  dutc <- P.take 5
  void $ P.take 2
  opoint <- portP           <?> "Leg off point"
  atime <- P.take 4
  void $ P.take 4
  autc <- P.take 5
  void $ P.take (2+3+20+5+10+9+2+6)
  iviH <- decimalP 1        <?> "Leg itinerary variation identifier (high)"
  void $ P.take (3+3+3+9+1+1+1+11+1+11+20)
  dvar <- P.anyChar
  avar <- P.anyChar
  void $ P.take 6
  void $ some P.endOfLine
  let variation = iviL + 100 * iviH
      flight = Flight airline fnum suffix
      period = Period bdate edate dow
      leg = Leg lsn bpoint opoint 
  return $ LegPeriod flight period variation leg

-- | Parser for fixed length decimal numbers with space padding.
decimalP :: Int -> Parser Int
decimalP n = do
  s <- P.take n
  let i = if B8.null stripped then Just (0, B8.empty) else B8.readInt stripped
          where stripped = B8.dropWhile (== ' ') s
  fromMaybe (fail ("Decimal parsing failed on " ++ show s))
          $ (return . fst) <$> i

-- | Parser for fixed length decimal numbers.
decimalP' :: Int -> Parser Int
decimalP' n = do
  i <- B8.readInt <$> P.take n
  fromMaybe (fail "Decimal parsing failed") $ (return . fst) <$> i

-- | Parser for days.
dayP :: Parser Int
dayP = decimalP' 2

-- | Parser for months.
monthP :: Parser Int
monthP = do
  m <- (flip elemIndex) months <$> P.take 3
  fromMaybe (fail "Month parsing failed") $ return <$> m
  where months = [ "XXX", "JAN","FEB", "MAR","APR", "MAY",
                   "JUN","JUL","AUG", "SEP", "OCT", "NOV", "DEC" ]

-- | Parser for years.
yearP :: Num a => Parser a
yearP = (fromIntegral . (2000+)) <$> decimalP' 2

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
fnumP = decimalP 4

-- | Parser for airline codes.
airlineP :: Parser AirlineCode
airlineP = sum <$> (sequence $ map step [0,1,2 :: Int]) <?> "Airline code"
  where step n = (* 37^n) <$> code n
        code n | n < 2     = letter <|>  digit
               | otherwise = letter <|>  digit <|> space
        letter = (+11) . subtract (ord 'A') . ord <$> P.satisfy (\c -> c >= 'A' && c <= 'Z')
        digit  =  (+1) . subtract (ord '0') . ord <$> P.digit
        space  = pure 0 <* P.space

-- | Parser for days of week.
dowP :: Parser Dow
dowP = sum <$> (sequence $ map step ['1'..'7']) <?> "Days of week"
  where step n = P.char n   *> (pure . bit $ ord n - ord '1')
             <|> P.char ' ' *> (pure 0)

-- | Parser for ports.
portP :: Parser Port
portP = sum <$> (sequence $ map step [0,1,2 :: Int]) <?> "Port"
  where step n = (* 26^n) . subtract (ord 'A') . ord <$> P.satisfy letter
        letter c = c >= 'A' && c <= 'Z'

-- | Parser for local times.
localTimeP :: Parser TimeOfDay
localTimeP = do
  m <- makeTimeOfDayValid <$> decimalP' 2 <*> decimalP' 2 <*> pure 0
  fromMaybe (fail "Local time parsing failed") $ return <$> m

-- | Parser for time variations.
timeVariationP :: Parser Int
timeVariationP = (plus <|> minus) <*> time
  where plus = P.char '+' *> pure id
        minus = P.char '-' *> pure negate
        time = (+) <$> ((*60) <$> decimalP' 2) <*> decimalP' 2

segmentP :: Parser Segment
segmentP = do
  void $ P.char '4'
  void $ P.take 199
  void $ some P.endOfLine
  return Segment

trailerP :: Parser ()
trailerP = (P.char '5'       <?> "Trailer record type")
        *> (P.take 199       <?> "Trailer padding")
        *> (some P.endOfLine <?> "Trailer separator")
        *> pure ()

legGroupP :: Parser LegGroup
legGroupP = LegGroup <$> (legPeriodP    <?> "Leg record")
                     <*> (some segmentP <?> "Leg segments")

carrierGroupP :: Parser CarrierGroup
carrierGroupP = CarrierGroup <$> (carrierP       <?> "Carrier record")
                             <*> (some legGroupP <?> "Carrier legs")

ssimP :: Parser Ssim
ssimP = Ssim <$> (headerP            <?> "SSIM7 header")
             <*> (some carrierGroupP <?> "SSIM7 carriers")
             <*  (trailerP           <?> "SSIM7 trailer")

resultLazy :: FP.FilePath -> IO (LP.Result Ssim)
resultLazy s = do
  c <- LB.readFile $ FP.encodeString s
  return $ LP.parse ssimP c

main :: IO ()
main = defaultMain [
  bench "lazy" $ whnfIO $ resultLazy input
  ]
  where input = "oag.ssim7.sample"
