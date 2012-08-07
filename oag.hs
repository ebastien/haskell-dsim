{-# LANGUAGE OverloadedStrings, NoMonomorphismRestriction #-}

import qualified Filesystem.Path.CurrentOS as FP

import Criterion.Main

import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy as LB

import Data.Attoparsec.ByteString (Parser, (<?>))
import qualified Data.Attoparsec.ByteString.Char8 as P
import qualified Data.Attoparsec.ByteString.Lazy as LP

import Data.Functor ((<$>))
import Control.Applicative (some, many, (<*>), (<*))

import Data.Maybe (fromMaybe)

import Data.Time.Calendar (Day, fromGregorianValid)

import Data.List (elemIndex)

type Designator = ByteString
type PeriodBoundary = Maybe Day

data Header = Header deriving Show

data Carrier = Carrier { carAirline :: Designator 
                       } deriving Show

data Leg = Leg { lgSuffix :: Char
               , lgAirline :: Designator
               , lgNumber :: Int
               , lgBegin :: PeriodBoundary
               , lgEnd :: PeriodBoundary
               } deriving Show

data Segment = Segment deriving Show

data LegGroup = LegGroup { lgLeg :: Leg
                         , lgSegments :: [Segment] } deriving Show

data CarrierGroup = CarrierGroup { cgCarrier :: Carrier
                                 , cgLegs :: [LegGroup] } deriving Show

data Ssim = Ssim { ssimHeader :: Header
                 , ssimCarriers :: [CarrierGroup] } deriving Show

headerP  = P.char '1' >> P.take 199 >> (some P.endOfLine) >> return Header

carrierP = do
  P.char '2'
  P.satisfy (\c -> c == 'U' || c == 'L')
  airline <- airlineP
  P.take 195
  some P.endOfLine
  return (Carrier airline)

legP = do
  P.char '3'                <?> "Leg record type"
  suffix <- P.anyChar
  airline <- airlineP
  fnum <- fnumP             <?> "Leg flight number"
  iviL <- decimalP 2        <?> "Leg itinerary variation identifier (low)"
  lsn <- decimalP 2         <?> "Leg sequence number"
  P.anyChar
  bdate <- periodBoundaryP  <?> "Leg period of operation (from)"
  edate <- periodBoundaryP  <?> "Leg period of operation (to)"
  dow <- P.take 7
  P.anyChar
  bpoint <- P.take 3
  P.take 4
  dtime <- P.take 4
  dutc <- P.take 5
  P.take 2
  opoint <- P.take 3
  atime <- P.take 4
  P.take 4
  autc <- P.take 5
  P.take (2+3+20+5+10+9+2+6)
  iviH <- decimalP 1        <?> "Leg itinerary variation identifier (high)"
  P.take (3+3+3+9+1+1+1+11+1+11+20)
  dvar <- P.anyChar
  avar <- P.anyChar
  P.take 6
  some P.endOfLine
  let pnum = iviL + 100 * iviH in
    return (Leg suffix airline fnum bdate edate)

-- | Parser for fixed length decimal numbers with space padding.
decimalP :: Int -> Parser Int
decimalP n = do
  s <- P.take n
  let i = if B8.null stripped then Just (0, B8.empty) else B8.readInt stripped
          where stripped = B8.dropWhile (== ' ') s
  fromMaybe (fail ("Decimal parsing failed on " ++ show s))
          $ (return . fst) <$> i

-- | Parser for days.
dayP :: Parser Int
dayP = decimalP 2

-- | Parser for months.
monthP :: Parser Int
monthP = do
  m <- (flip elemIndex) l <$> P.take 3
  fromMaybe (fail "Month parsing failed") $ return <$> m
  where l = [ "XXX", "JAN","FEB", "MAR","APR", "MAY",
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
fnumP = decimalP 4

-- | Parser for airline designators.
airlineP = P.take 3

segmentP = P.char '4' >> P.take 199 >> (some P.endOfLine) >> return Segment

trailerP = do
  P.char '5'       <?> "Trailer record type"
  P.take 199       <?> "Trailer padding"
  some P.endOfLine <?> "Trailer separator"
  return ()

legGroupP = LegGroup <$> (legP          <?> "Leg record")
                     <*> (some segmentP <?> "Leg segments")

carrierGroupP = CarrierGroup <$> (carrierP       <?> "Carrier record")
                             <*> (some legGroupP <?> "Carrier legs")

ssimP = Ssim <$> (headerP            <?> "SSIM7 header")
             <*> (some carrierGroupP <?> "SSIM7 carriers")
             <*  (trailerP           <?> "SSIM7 trailer")

resultLazy :: FP.FilePath -> IO (LP.Result Ssim)
resultLazy s = do
  c <- LB.readFile $ FP.encodeString s
  return $ LP.parse ssimP c

main = defaultMain [
  bench "lazy" $ whnfIO $ resultLazy input
  ]
  where input = "oag.ssim7.sample"
