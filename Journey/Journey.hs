
import Data.Array.IArray
import Data.Tuple (swap)
import Control.Monad (guard)

import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Vector as V

type Port = Int

type OnD = (Port, Port)

type Path = [Port]

type PortBounds = (Port, Port)

type Distance = Double

class NormedSpace a where
  distance :: a -> a -> Distance

data Move a = Move Distance a deriving Show

data Outbound a = Outbound Path Port [Move a] deriving Show

type PortsCoverage a = Array Port [Outbound a]

type PortsAdjacency a = Array Port [(Port, a, a, Distance)]

-- | Group a list of pairs by port, generating an array
group_by_port :: PortBounds -> [(Port, a)] -> Array Port [a]
group_by_port = accumArray (flip (:)) []

-- | Predicate for competitive itinerary according to direct vs. indirect distances
competitive_itinerary :: (NormedSpace a) => a -> Distance -> [Move a] -> Bool
competitive_itinerary coord0 dist0 moves = all competitive steps
  where competitive (indirect, direct) = (indirect * ratio) < direct
        ratio = 0.6
        steps = scanl compose (dist0, dist0) moves
        compose (indirect, _) (Move dist coord) = (indirect', direct)
          where indirect' = indirect + dist
                direct = distance coord0 coord

-- | Extend the coverage by one more stop
extend_coverage :: (NormedSpace a) => PortsAdjacency a -> PortsCoverage a -> PortsCoverage a
extend_coverage adj cov | bnds == bounds adj = cov'
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

-- | Direct ports coverage from adjacency list
direct_coverage :: (NormedSpace a) => PortsAdjacency a -> PortsCoverage a
direct_coverage adj = group_by_port (bounds adj) $ concatMap adj_to_cov $ assocs adj
  where adj_to_cov (dst, adjs) = do
          (org, _, coord_dst, distance) <- adjs
          let move = Move distance coord_dst
          return (org, Outbound [] dst [move])

-- | List of all coverages in path length order
coverages :: (NormedSpace a) => PortsAdjacency a -> [PortsCoverage a]
coverages adj = iterate (extend_coverage adj) (direct_coverage adj)

--

newtype GeoCoord = GeoCoord (Double, Double) deriving Show

orthodromic_distance :: GeoCoord -> GeoCoord -> Distance
orthodromic_distance _ _ = 1.0

instance NormedSpace GeoCoord where
  distance = orthodromic_distance

type PortsSet = Array Port GeoCoord

loadPorts :: String -> IO PortsSet
loadPorts f = return . accumArray (flip const) def (0,1) . map parse . drop 1 . T.lines =<< T.readFile f
  where def = GeoCoord (0.0,0.0)
        parse row = (port, GeoCoord (lat, lon))
          where col = V.fromList $ T.split (=='^') row
                port = undefined -- col V.! 0
                lat = read . T.unpack $ col V.! 7
                lon = read . T.unpack $ col V.! 8

-- | Ports adjacency in geographic coordinates
adjacency :: PortsSet -> [OnD] -> PortsAdjacency GeoCoord
adjacency ports onds = group_by_port (bounds ports) $ map make_edge onds
  where make_edge (org, dst) = (dst, (org, coord_org, coord_dst, dist))
          where dist = distance coord_org coord_dst
                coord_org = (ports ! org)
                coord_dst = (ports ! dst)

-- 

main = do
  ports <- loadPorts "ports.csv"
  let adj = adjacency ports [(0,1)]
      cov = take 3 $ coverages adj
  putStrLn $ show cov

