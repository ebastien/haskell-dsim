{-# LANGUAGE OverloadedStrings #-}

module Journey
    () where
    
import Data.Array.IArray
import Data.Tuple (swap)
import Control.Monad (guard, join)
import Data.Maybe (fromJust)
import Control.Arrow ((***))

import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Text.Encoding as T
import qualified Data.Vector as V
import qualified Data.IntMap as M

import Ssim (Port, toPort)

{-
  Ports
-}

-- | Origin & Destination.
type OnD = (Port, Port)

-- | A sequence of ports.
type Path = [Port]

-- | A sorted collection of port associations.
newtype PortMap a = MkPortMap (M.IntMap a)

instance (Show a) => Show (PortMap a) where
  show = show . toPortAssocs

-- | Create a collection from a list of port associations.
fromPortAssocs :: [(Port, a)] -> PortMap a
fromPortAssocs = MkPortMap . M.fromListWith const . map assoc
  where assoc (p, v) = (fromEnum p, v)

-- | Group a list of associations by port creating a collection.
groupByPort :: [(Port, a)] -> PortMap [a]
groupByPort = MkPortMap . M.fromListWith (++) . map assoc
  where assoc (p, v) = (fromEnum p, [v])

-- | Convert a collection to a sorted list of port associations.
toPortAssocs :: PortMap a -> [(Port, a)]
toPortAssocs (MkPortMap m) = map assoc $ M.assocs m
  where assoc (p, v) = (toEnum p, v)

-- | Find the element associated to the given port.
findByPort :: PortMap a -> Port -> a
findByPort (MkPortMap m) p = m M.! (fromEnum p)

toPorts :: PortMap a -> [Port]
toPorts (MkPortMap m) = map toEnum $ M.keys m

{-
  Metric space
-}

-- | A set of elements with a notion of distance.
class MetricSpace e where
  -- | Distance between two elements.
  distance :: e -> e -> Distance

-- | A distance between elements of the metric space.
type Distance = Double

{-
  Graph of ports
-}

-- | A coverage by origin port.
type PortsCoverage e = PortMap [Itinerary e]

-- | An itinerary.
data Itinerary e = Itinerary
    Path      -- via ports
    Port      -- final destination
    [Step e]  -- list of steps
    deriving (Show)

-- | A step in the graph of ports.
data Step e = Step
    Distance  -- distance
    e         -- destination element
    deriving (Show)

-- | A graph of ports organized as adjacency by destination.
type PortsAdjacency e = PortMap [Edge e]

-- | An edge of the graph of ports.
data Edge e = Edge
    Port      -- origin port
    e         -- origin element
    e         -- destination element
    Distance  -- distance between origin and destination
    deriving (Show)

-- | Predicate for competitive itinerary according to direct vs. indirect distances.
isCompetitive :: (MetricSpace e) => e -> Distance -> [Step e] -> Bool
isCompetitive elem0 dist0 steps = all competitive itineraries
  where competitive (indirect, direct) = indirect < ratio * direct
        ratio = 10
        itineraries = scanl compose (dist0, dist0) steps
        compose (indirect, _) (Step dist elem) = (indirect', direct)
          where indirect' = indirect + dist
                direct = distance elem0 elem

-- | Extend the coverage by one more step.
extendedCoverage :: (MetricSpace e) => PortsAdjacency e -> PortsCoverage e -> PortsCoverage e
extendedCoverage adj cov = groupByPort outbounds
  where outbounds = concatMap extend $ zjoin (toPortAssocs cov) (toPortAssocs adj)
        extend (port, covs, adjs) = do
          Edge port0 elem0 elem dist0 <- adjs
          Itinerary path dest steps <- covs
          guard $ isCompetitive elem0 dist0 steps
          let path' = port : path
              steps' = (Step dist0 elem) : steps
          return (port0, Itinerary path' dest steps')

-- | Join two lists of pairs sorted by the first element.
zjoin :: (Ord a) => [(a,b)] -> [(a,c)] -> [(a,b,c)]
zjoin [] ys = []
zjoin xs [] = []
zjoin x@((xa,xb):xs) y@((ya,yb):ys) | xa > ya   = zjoin x ys
                                    | xa < ya   = zjoin xs y
                                    | otherwise = (xa,xb,yb) : zjoin xs ys

-- | Direct ports coverage from adjacency list.
directCoverage :: (MetricSpace e) => PortsAdjacency e -> PortsCoverage e
directCoverage adj = groupByPort $ concatMap adj_to_cov (toPortAssocs adj)
  where adj_to_cov (port, adjs) = [ (port0, Itinerary [] port [Step dist0 elem])
                                  | Edge port0 _ elem dist0 <- adjs ]

-- | List of all coverages in path length order.
coverages :: (MetricSpace e) => PortsAdjacency e -> [PortsCoverage e]
coverages adj = iterate (extendedCoverage adj) (directCoverage adj)

{-
  Geographic coordinates space
-}

newtype GeoCoord = GeoCoord (Double, Double) deriving (Show)

-- | The orthodromic distance between two geographic coordinates.
orthodromicDistance :: GeoCoord -> GeoCoord -> Distance
orthodromicDistance _ _ = 1.0

instance MetricSpace GeoCoord where
  distance = orthodromicDistance

type PortsInfo = PortMap GeoCoord

-- | Load ports information from a file.
loadPorts :: String -> IO PortsInfo
loadPorts f = return . fromPortAssocs . map parse . drop 1 . T.lines =<< T.readFile f
  where parse row = (port, GeoCoord (lat, lon))
          where col = V.fromList $ T.split (=='^') row
                port = fromJust . toPort . T.encodeUtf8 $ col V.! 0
                lat = read . T.unpack $ col V.! 7
                lon = read . T.unpack $ col V.! 8

-- | Ports adjacency in geographic coordinates.
adjacency :: PortsInfo -> [OnD] -> PortsAdjacency GeoCoord
adjacency ports onds = groupByPort $ map make_edge onds
  where make_edge (org, dst) = (dst, Edge org coord_org coord_dst dist)
          where dist = distance coord_org coord_dst
                coord_org = findByPort ports org
                coord_dst = findByPort ports dst

{-
  Entry point
-}

onds = [("NCE", "CDG")
       ,("CDG", "FRA")
       ,("FRA", "JFK")
       ,("CDG", "JFK")]

main = do
  ports <- loadPorts "ports.csv"
  let adj = adjacency ports $ map (join (***) (fromJust . toPort)) onds
      cov = take 3 $ coverages adj
  mapM_ (putStrLn . show) cov

