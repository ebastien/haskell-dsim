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
 
generateBookingRequest :: (RandomGen g) => g -> (g, BookingRequest)
generateBookingRequest g = (g, BookingRequest)

simulateBookingRequest :: BookingRequest -> TravelSolution
simulateBookingRequest br = TravelSolution

findTravelSolutions :: BookingRequest -> TravelSolutionsList
findTravelSolutions br = []

getNextBookingRequest :: IO BookingRequest
getNextBookingRequest = do
  gen <- getStdGen
  let (newgen, br) = generateBookingRequest gen
  setStdGen newgen
  return br

main = do
  br <- getNextBookingRequest
  let ts = simulateBookingRequest br
  return ()
