{-# LANGUAGE OverloadedStrings #-}

module Ssim (
      readSsimFile
    , ssimOnDs
    ) where

import qualified Data.ByteString.Lazy as LB

import Data.Attoparsec.ByteString (Parser, (<?>))
import qualified Data.Attoparsec.ByteString.Char8 as P
import qualified Data.Attoparsec.ByteString.Lazy as LP

import Data.Functor ((<$>))
import Control.Monad (void, join)
import Control.Applicative (pure, some, (<*>), (<*), (*>))

import Data.Maybe (fromMaybe, mapMaybe)

import Data.List (sort, group, groupBy)
import Data.Function (on)

import Data.Time.Calendar (Day)

import qualified EnumMap as M

import Types

data Header = Header deriving Show

data Carrier = Carrier { cAirline :: !AirlineCode 
                       } deriving Show

data Flight = Flight { fAirline :: !AirlineCode
                     , fNumber :: !Int
                     , fSuffix :: !Char
                     , fVariation :: !Int
                     } deriving (Show, Eq)

data LegPeriod = LegPeriod { lpFlight :: Flight
                           , lpBegin :: !PeriodBoundary
                           , lpEnd :: !PeriodBoundary
                           , lpDow :: !Dow
                           , lpSequence :: !Int
                           , lpBoard :: !Port
                           , lpOff :: !Port
                           , lpDepartureTime :: !ScheduleTime
                           , lpArrivalTime :: !ScheduleTime
                           , lpElapsedTime :: !TimeDuration
                           } deriving Show

type SegmentDEI = Int

data SegmentData = SegmentData { sdFlight :: Flight
                               , sdIndex :: !Int
                               , sdBoard :: !Port
                               , sdOff :: !Port
                               , sdDEI :: !SegmentDEI
                               } deriving Show

data LegGroup = LegGroup { lgLeg :: LegPeriod
                         , lgSegments :: [SegmentData] } deriving Show

type FlightGroup = [LegGroup]

data CarrierGroup = CarrierGroup { cgCarrier :: Carrier
                                 , cgFlights :: [FlightGroup] } deriving Show

data Ssim = Ssim { ssimHeader :: Header
                 , ssimCarriers :: [CarrierGroup] } deriving Show

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
      flight = Flight airline fnum suffix variation
      etime = (atime - atvar + advar) - (dtime - dtvar + ddvar)
  return $ LegPeriod flight bdate edate dow lsn bpoint opoint dtime atime etime

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

{-------------------------------------------------------------------------------
  Segments browsing
-------------------------------------------------------------------------------}

data POnD = MkPOnD {- #UNPACK# -} !Port
                   {- #UNPACK# -} !Port
                   deriving (Show)

instance Enum POnD where
  fromEnum (MkPOnD a b) = (fromEnum a) * 26^3 + (fromEnum b)
  toEnum i = let (a,b) = divMod i (26^3) in MkPOnD (toEnum a) (toEnum b)

-- | A collection of OnD associations.
type OnDMap a = M.EnumMap POnD a

type Segment = [(LegPeriod, [SegmentDEI])]

type OnDSegments = OnDMap [Segment]

-- | Segment index from board and off legs
segmentIdx :: LegPeriod -> LegPeriod -> Int
segmentIdx a b = (lpSequence b) * 26 + (lpSequence a) - 1

-- | Extract segments from a flight
flightSegments :: FlightGroup -> [(POnD, Segment)]
flightSegments = join . combine
  where combine []         = []
        combine xs@(x:xs') = [ mkAssoc y | y <- xs ] : combine xs'
          where legX = lgLeg x
                mkAssoc y = (MkPOnD (lpBoard legX) (lpOff legY), map select legs)
                  where legY = lgLeg y
                        legs = takeWhile (on (<) (lpSequence . lgLeg) $ y) xs ++ [y]
                        select l = (legL, map sdDEI . filter ((==) idx . sdIndex) $ lgSegments l)
                          where legL = lgLeg l
                                idx = segmentIdx legL legY

-- | Extract segments from a SSIM structure.
ssimSegments :: Ssim -> OnDSegments
ssimSegments ssim = M.group $ do
  car <- ssimCarriers ssim
  flt <- cgFlights car
  flightSegments flt

composePath :: OnDSegments -> Path -> [[Segment]]
composePath onds = walk [id]
  where walk done (b:[])   = map ($[]) done
        walk done (a:b:ps) = walk done' (b:ps)
          where done' = [ d . (c:) | c <- choices, d <- done ]
                choices = M.find (MkPOnD a b) onds

data SegmentDate = SegmentDate { sFlight :: Flight
                               , sDate :: !Day
                               , sBoard :: !Port
                               , sOff :: !Port
                               , sDepartureTime :: !ScheduleTime
                               , sArrivalTime :: !ScheduleTime
                               , sElapsedTime :: !TimeDuration
                               } deriving Show

fromSegment :: Day -> Segment -> Maybe SegmentDate
fromSegment d s = undefined

connections :: Day -> OnDSegments -> Path -> [[SegmentDate]]
connections d0 segs p = mapMaybe alternative $ composePath segs p
  where alternative xs = case fromSegment d0 (head xs) of
                           Just first -> walk d0 (first:) xs
                           Nothing    -> Nothing
        walk _ done (b:[])   = Just $ done []
        walk d done (a:b:xs) = case connect d a b of
                                 Just (next, d') -> walk d' (done . (next:)) (b:xs)
                                 Nothing         -> Nothing
        connect d a b = undefined

-- | Extract OnDs from a flight.
flightOnDs :: FlightGroup -> [OnD]
flightOnDs = join . combine . map lgLeg
  where combine xs@(x:xs') = [ (lpBoard x, lpOff y) | y <- xs ] : combine xs'
        combine []        = []

-- | Extract unique OnDs from a SSIM structure.
ssimOnDs :: Ssim -> [OnD]
ssimOnDs ssim = map head . group . sort $ do
  car <- ssimCarriers ssim
  flt <- cgFlights car
  flightOnDs flt

-- | Run the SSIM parser on the given file.
readSsimFile :: String -> IO Ssim
readSsimFile s = do
  result <- LP.parse ssimP <$> LB.readFile s
  case result of
    LP.Fail rem ctx msg -> fail . unlines $ msg:(show $ LB.length rem):ctx
    LP.Done _ ssim      -> return ssim
