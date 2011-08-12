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

data BreakPoint = NeverBreak | TimeBreak { breakTime :: UTCTime } deriving (Show)

-- | Generate a single event

generateEvent :: (RandomGen gen) => gen -> Event -> (Event, gen)
generateEvent gen0 evt0 = (Event newTime newType, gen0)
                        where
                          newTime = addUTCTime (fromInteger 10) (eventTime evt0)
                          newType = BookingEvent BookingRequest

-- | Generate the demand (i.e an infinite sequence of events)

generateDemand :: (RandomGen gen) => gen -> Event -> Demand
generateDemand gen0 evt0 = let (evt1, gen1) = generateEvent gen0 evt0 in evt1:generateDemand gen1 evt1

-- | Apply a single event on a simulation snapshot

applyEvent :: Snapshot -> Event -> Snapshot
applyEvent (Snapshot tp c) (Event te _) = Snapshot te (c+1)

-- | Test whether a breakpoint has been reached

breakReached :: BreakPoint -> Snapshot -> Bool
breakReached NeverBreak    _ = False
breakReached (TimeBreak t) p = t <= (snapTime p)

-- | Simulate a demand starting from an initial snapshot

runSimulation :: Snapshot -> Demand -> Simulation
runSimulation p0 d = scanl applyEvent p0 d

-- | Run a simulation until the breakpoint is reached

runUntil :: BreakPoint -> Snapshot -> Demand -> Simulation
runUntil b p0 d = takeWhile (not . breakReached b) $ runSimulation p0 d

--

tzero = UTCTime (fromGregorian 2000 01 01) (timeOfDayToTime (TimeOfDay 00 00 00))
tdone = UTCTime (fromGregorian 2000 01 01) (timeOfDayToTime (TimeOfDay 00 01 00))
snap = Snapshot tzero 0
bkpt = TimeBreak tdone
demand = generateDemand (mkStdGen 123) (Event tzero VoidEvent)
result = runUntil bkpt snap demand
