
import Data.Array.IArray
import Data.Tuple (swap)
import Control.Monad (guard)

data FlightKey = FlightKey {
}

data SegmentPeriod = SegmentPeriod {
  spKey :: FlightKey,
  spSegment :: Segment
}

data Segment = Segment {
  sgKey :: SegmentKey,
  sgLegs :: SegmentLegs
}

type SegmentKey = OnD

data SegmentLegs = SegmentLegs {
  slBoard :: Leg,
  slVia :: [Leg],
  slOff :: Leg
}

data Leg = Leg {
  lgKey :: LegKey
}

type LegKey = OnD

type OnD = (Port, Port)

type Port = Int

-- 

type Distance = Double

type Coord = (Double, Double)

data Move = Move Distance Coord deriving Show

type Path = [Port]

data Outbound = Outbound Path Port [Move] deriving Show

data Inbound = Inbound Port Coord Move deriving Show

type PortBounds = (Port, Port)

type PortsCoverage = Array Port [Outbound]

type PortsAdjacency = Array Port [(Port, Coord, Coord, Distance)]

group_by_port :: PortBounds -> [(Port, a)] -> Array Port [a]
group_by_port bnds = accumArray (flip (:)) [] bnds

coordinates :: Port -> Coord
coordinates _ = (0.0, 0.0)

orthodromic_distance :: Coord -> Coord -> Distance
orthodromic_distance _ _ = 1.0

-- Predicate for competitive itinerary according to direct vs. indirect distances
competitive_itinerary :: Coord -> Distance -> [Move] -> Bool
competitive_itinerary coord0 dist0 moves = all competitive steps
  where competitive (indirect, direct) = (indirect * ratio) < direct
        ratio = 0.6
        steps = scanl compose (dist0, dist0) moves
        compose (indirect, _) (Move dist coord) = (indirect', direct)
          where indirect' = indirect + dist
                direct = orthodromic_distance coord0 coord

-- Extend the coverage by one more stop
extend_coverage :: PortsCoverage -> PortsAdjacency -> PortsCoverage
extend_coverage cov adj | bnds == bounds adj = cov'
  where bnds = bounds cov
        cov' = group_by_port bnds outbounds
        outbounds = concatMap extend $ zip (assocs cov) (assocs adj)
        extend ((port,covs),(_,adjs)) = do
          (org, coord_org, coord_port, distance) <- adjs
          Outbound stops dst moves <- covs
          guard $ competitive_itinerary coord_org distance moves
          let stops' = port:stops
              moves' = (Move distance coord_port):moves
          return (org, Outbound stops' dst moves')

-- Initial ports coverage from adjacency list
initial_coverage :: PortsAdjacency -> PortsCoverage
initial_coverage adj = group_by_port (bounds adj) $ concatMap adj_to_cov $ assocs adj
  where adj_to_cov (dst, adjs) = do
          (org, _, coord_dst, distance) <- adjs
          let move = Move distance coord_dst
          return (org, Outbound [] dst [move])

-- Ports adjacency from direct OnDs
adjacency :: PortBounds -> [OnD] -> PortsAdjacency
adjacency bnds onds = group_by_port bnds $ map make_edge onds
  where make_edge (org, dst) = (dst, (org, coord_org, coord_dst, distance))
          where distance = orthodromic_distance coord_org coord_dst
                coord_org = coordinates org
                coord_dst = coordinates dst

p0 = 0
p1 = 1
p2 = 2
p3 = 3

onds = [
       (p0, p1),
       (p2, p3),
       (p3, p0),
       (p3, p1)
       ] :: [OnD]

bnds = (p0, p3)

-- 

main = return ()
