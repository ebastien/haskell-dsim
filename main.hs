import DSim.Inventory
import Data.Time.Calendar
import Data.Time.LocalTime

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

data Event = BookingRequestEvent BookingRequest

type Demand = [Event]

data Snapshot = VoidSnapshot

generateEvent :: (RandomGen gen) => gen -> (Event, gen)
generateEvent gen = (BookingRequestEvent BookingRequest, gen)

generateDemand :: (RandomGen gen) => gen -> Demand
generateDemand gen0 = let (e, gen1) = generateEvent gen0 in e:generateDemand gen1

simulateBookingRequest :: BookingRequest -> TravelSolution
simulateBookingRequest br = TravelSolution

findTravelSolutions :: BookingRequest -> TravelSolutionsList
findTravelSolutions br = []

runEvent :: Event -> Snapshot -> Snapshot
runEvent e s = s

runSimulation :: Demand -> Snapshot -> Snapshot
runSimulation d s0 = foldr runEvent s0 d

main = do
  gen <- getStdGen
  let demand = take 10 $ generateDemand gen
  return $ runSimulation demand VoidSnapshot
