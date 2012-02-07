
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

data Move = Move Distance Coord

data OutboundPath = OutboundPath {
  opVia :: Path,
  opOff :: Port,
  opMoves :: [Move]
}

type Path = [Port]

type PortOutbounds = [(Port, [OutboundPath])]

type PortAdjacency = [(Port, [Port])]

type PortGraph = (PortOutbounds, PortAdjacency)

type OnDPaths = Array OnD [Path]

ond_bounds = ((0,0),(3,3))

onds_direct :: PortOutbounds -> OnDPaths
onds_direct outbounds = array ond_bounds assos
  where assos = do
          (org, [outpath]) <- outbounds
          let dst = opOff outpath
          return ((org,dst), [[]]) -- a single direct (i.e empty stops list) path from org to dst

p1 = 0
p2 = 1
p3 = 3

po = [
     (p1, [ (OutboundPath [], p2, []) ]),
     (p2, [ (OutboundPath [], p3, []) ]),
     (p3, [ (OutboundPath [], p1, []) ])
     ]

-- 

main = return ()
