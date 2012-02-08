
import Data.Array.IArray
import Data.Tuple

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

data Outbound = Outbound {
  opVia :: Path,
  opOff :: Port,
  opMoves :: [Move]
} deriving Show

type Path = [Port]

type PortsCoverage = Array Port [Outbound]

type PortsAdjacency = Array Port [Port]

port_bounds = (0,3)

group_by_port :: [(Port, a)] -> Array Port [a]
group_by_port = accumArray (flip (:)) [] port_bounds

-- Extend the coverage by one more stop
extend_coverage :: PortsCoverage -> PortsAdjacency -> PortsCoverage
extend_coverage cov adj | bounds cov == bounds adj = cov'
  where cov' = group_by_port outbounds
        outbounds = concatMap extend $ zip (assocs cov) (assocs adj)
        extend ((p,cs),(_p,as)) = do
          org <- as
          Outbound via dst _ <- cs
          let outbound' = Outbound (p:via) dst []
          return (org, outbound')

-- Initial ports coverage from direct OnDs
direct_coverage :: [OnD] -> PortsCoverage
direct_coverage onds = group_by_port $ map outbound onds
  where outbound (org,dst) = (org, Outbound [] dst [])

-- Ports adjacency from direct OnDs
adjacency :: [OnD] -> PortsAdjacency
adjacency onds = group_by_port $ map swap onds

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

-- 

main = return ()
