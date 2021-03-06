module DSim.Inventory (
  -- * Leg
  LegDate(..),
  -- * Segment
  SegmentDate(..),
  -- * Flight
  FlightDate(..),
  -- * Inventory
  Inventory(..),
  insertFlight,
  ) where

import qualified Data.Time.Calendar as Cal
import qualified Data.Time.LocalTime as Ltm
import qualified Data.Time.Format as Fmt
import qualified Data.Foldable as F

type Date = Cal.Day
type Time = Ltm.TimeOfDay
type FlightNumber = Int
type AirlineCode = String
type AirportCode = String

data LegDate = LegDate { legBoardingPoint :: AirportCode
                       , legOffPoint :: AirportCode
                       , legBoardingDate :: Date
                       , legBoardingTime :: Time
                       , legOffDate :: Date
                       , legOffTime :: Time
                       } deriving (Show)

data SegmentDate = SegmentDate { segLegs :: [LegDate]
                               } deriving (Show)

segDepartureDate s = legBoardingDate . head $ segLegs s
segDepartureTime s = legBoardingTime . head $ segLegs s
segArrivalDate s = legOffDate . last $ segLegs s
segArrivalTime s = legOffTime . last $ segLegs s

data FlightDate = FlightDate { fltNumber :: FlightNumber
                             , fltLegs :: [LegDate]
                             , fltSegments :: [SegmentDate]
                             } deriving (Show)

fltDepartureDate f = legBoardingDate . head $ fltLegs f
fltDepartureTime f = legBoardingTime . head $ fltLegs f
fltArrivalDate f = legOffDate . last $ fltLegs f
fltArrivalTime f = legOffTime . last $ fltLegs f

data Inventory flights = Inventory { invAirline :: AirlineCode
                                   , invFlights :: [flights]
                                   } deriving (Show)

-- We can fold over the flights of an inventory
instance F.Foldable Inventory where
  foldMap f i = F.foldMap f (invFlights i)

insertFlight :: Inventory f -> f -> Inventory f
insertFlight (Inventory air flts) f = Inventory air (f:flts)

