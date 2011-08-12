import DSim.Inventory

import Data.Time.Calendar
import Data.Time.LocalTime
import Data.Time.Clock

import Random

l1 = LegDate "NCE" "CDG"
             (fromGregorian 2010 12 01)
             (TimeOfDay 15 35 00)
             (fromGregorian 2010 12 01)
             (TimeOfDay 16 40 00)

s1 = SegmentDate [l1]

f1 = FlightDate 3765 [l1] [s1]

i1 = Inventory "AF" [f1]


data BookingRequest = BookingRequest {} deriving (Show)

data TravelSolution = TravelSolution {} deriving (Show)

type TravelSolutionsList = [TravelSolution]

type Fare = Double

type Availability = Double

simulateBookingRequest :: BookingRequest -> TravelSolution
simulateBookingRequest br = TravelSolution

findTravelSolutions :: BookingRequest -> TravelSolutionsList
findTravelSolutions br = []

data EventType = VoidEvent | BookingEvent BookingRequest deriving (Show)

data Event = Event { eventTime :: UTCTime
                   , eventType :: EventType
                   } deriving (Show)

type Demand = [Event]

data Snapshot = Snapshot { snapTime :: UTCTime
                         , snapCounter :: Int
                         } deriving (Show)

type Simulation = [Snapshot]

data BreakPoint = TimeBreak { breakTime :: UTCTime } deriving (Show)

-- | Generate a single event

generateEvent :: (RandomGen gen) => gen -> UTCTime -> (Event, gen)
generateEvent gen0 t0 = (Event t1 be, gen0)
                      where
                        t1 = (fromInteger 10) `addUTCTime` t0
                        be = BookingEvent BookingRequest

-- | Generate the demand (i.e an infinite sequence of events)

generateDemand :: (RandomGen gen) => gen -> UTCTime -> Demand
generateDemand gen0 t0 = e1 : generateDemand gen1 (eventTime e1)
                       where
                         (e1, gen1) = generateEvent gen0 t0

-- | Apply a single event on a simulation snapshot

applyEvent :: Snapshot -> Event -> Snapshot
applyEvent (Snapshot tp c) (Event te _) = Snapshot te (c+1)

-- | Test whether a breakpoint has been reached

bkptReached :: BreakPoint -> Snapshot -> Bool
bkptReached (TimeBreak t) p = t <= (snapTime p)

-- | Simulate a demand starting from an initial snapshot

runSimulation :: Snapshot -> Demand -> Simulation
runSimulation p0 d = scanl applyEvent p0 d

-- | Keep simulation results until a breakpoint is reached

keepUntil :: BreakPoint -> Simulation -> Simulation
keepUntil b = takeWhile (not . bkptReached b)

-- | Keep simulation results between two breakpoints

keepBetween :: BreakPoint -> BreakPoint -> Simulation -> Simulation
keepBetween b1 b2 = dropWhile (not . bkptReached b1) . keepUntil b2

-- The starting time of the simulation
t_zero = UTCTime (fromGregorian 2000 01 01) (timeOfDayToTime (TimeOfDay 00 00 00))

-- The initial state of the simulation
s_zero = Snapshot t_zero 0

demand = generateDemand (mkStdGen 123) t_zero

b1 = TimeBreak $ UTCTime (fromGregorian 2000 03 01) (timeOfDayToTime (TimeOfDay 00 10 00))
b2 = TimeBreak $ UTCTime (fromGregorian 2000 03 01) (timeOfDayToTime (TimeOfDay 00 11 00))

result = keepBetween b1 b2 $ runSimulation s_zero demand

main = do
  putStrLn $ show result
