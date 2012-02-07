
import Data.Array.IArray

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

type PortsCoverage = [(Port, [Outbound])]

type PortsAdjacency = [(Port, [Port])]

type OnDPaths = Array OnD [Path]

port_bounds = (0,3)
ond_bounds = ((0,0),(3,3))

onds_direct :: PortsCoverage -> OnDPaths
onds_direct coverages = array ond_bounds assos
  where assos = do
          (org, outbounds) <- coverages
          path <- outbounds
          let dst = opOff path
          return ((org,dst), [[]]) -- a single direct (i.e empty stops list) path from org to dst

onds_ext :: PortsCoverage -> PortsAdjacency -> Array Port [Outbound]
onds_ext coverages adjacencies = accumArray (flip (:)) [] port_bounds $ join coverages adjacencies
  where join cs@((p, c):cs') as@((p', a):as') | p == p' = (extend p c a) ++ (join cs' as')
                                              | p < p' = join cs' as
                                              | p > p' = join cs as'
        join [] _ = []
        join _ [] = []
        extend stop coverage adjacency = do
          org <- adjacency
          Outbound via dst _ <- coverage
          let outbound' = Outbound (stop:via) dst []
          return (org, outbound')

p0 = 0
p1 = 1
p2 = 2
p3 = 3

po = [
     (p0, [
          (Outbound [] p1 []),
          (Outbound [] p3 [])
          ]),
     (p1, [ (Outbound [] p2 []) ]),
     (p2, [ (Outbound [] p0 []) ]),
     (p3, [])
     ]

-- 

main = return ()
