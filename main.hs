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

type SimTime = NominalDiffTime

data Event = Event { eventTime :: SimTime
                   , eventType :: EventType
                   } deriving (Show)

type Demand = [Event]

data Snapshot = Snapshot { snapTime :: SimTime
                         , snapCounter :: Int
                         } deriving (Show)

type Simulation = [Snapshot]

data BreakPoint = TimeBreak { breakTime :: SimTime } deriving (Show)

-- | Generate a single event

generateEvent :: (RandomGen gen) => gen -> SimTime -> (Event, gen)
generateEvent gen0 t0 = (Event t1 be, gen0)
                      where
                        t1 = {-# SCC "A" #-} t0 + 10
                        be = {-# SCC "B" #-} BookingEvent BookingRequest

-- | Generate the demand (i.e an infinite sequence of events)

generateDemand :: (RandomGen gen) => gen -> SimTime -> Demand
generateDemand gen0 t0 = e1 : generateDemand gen1 (eventTime e1)
                       where
                         (e1, gen1) = generateEvent gen0 t0

-- | Apply a single event on a simulation snapshot

applyEvent :: Snapshot -> Event -> Snapshot
applyEvent (Snapshot tp c) (Event te _) = Snapshot te (c+1)

-- | Test whether a breakpoint has been reached

bkptReached :: BreakPoint -> Snapshot -> Bool
bkptReached (TimeBreak t) p = t <= (snapTime p)

-- | Strict version of scanl

scanl' :: (a -> b -> a) -> a -> [b] -> [a]
scanl' f q ls =  q : (case ls of
                      []   -> []
                      x:xs -> let q' = f q x in q' `seq` scanl' f q' xs)

-- | Simulate a demand starting from an initial snapshot

runSimulation :: Snapshot -> Demand -> Simulation
runSimulation p0 d = scanl' applyEvent p0 d

-- | Keep simulation results until a breakpoint is reached

keepUntil :: BreakPoint -> Simulation -> Simulation
keepUntil b = takeWhile (not . bkptReached b)

-- | Keep simulation results between two breakpoints

keepBetween :: BreakPoint -> BreakPoint -> Simulation -> Simulation
keepBetween b1 b2 = dropWhile (not . bkptReached b1) . keepUntil b2

-- The starting time of the simulation
t_zero = UTCTime (fromGregorian 2000 01 01) (timeOfDayToTime (TimeOfDay 00 00 00))

-- The initial state of the simulation
s_zero = Snapshot 0 0

demand = generateDemand (mkStdGen 123) 0

t1 = UTCTime (fromGregorian 2000 03 01) (timeOfDayToTime (TimeOfDay 00 10 00))
t2 = UTCTime (fromGregorian 2000 03 01) (timeOfDayToTime (TimeOfDay 00 11 00))
b1 = TimeBreak $ diffUTCTime t1 t_zero
b2 = TimeBreak $ diffUTCTime t2 t_zero

result = keepBetween b1 b2 $ runSimulation s_zero demand

main = do
  putStrLn $ show result
