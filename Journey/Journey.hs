{-# LANGUAGE OverloadedStrings #-}

module Journey
    () where
    
import Data.Array.IArray
import Data.Tuple (swap)
import Control.Monad (guard)
import Data.Maybe (fromJust)

import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Text.Encoding as T
import qualified Data.Vector as V
import qualified Data.IntMap as M

import Ssim (Port, toPort)

type OnD = (Port, Port)

type Path = [Port]

type PortBounds = (Port, Port)

type Distance = Double

class NormedSpace a where
  distance :: a -> a -> Distance

data Move a = Move Distance a deriving Show

data Outbound a = Outbound Path Port [Move a] deriving Show

newtype PortMap a = MkPortMap (M.IntMap a) deriving Show

-- | Create a map from a list of port associations.
fromPortAssocs :: [(Port, a)] -> PortMap a
fromPortAssocs = MkPortMap . M.fromListWith const . map assoc
  where assoc (p, v) = (fromEnum p, v)

-- | Group a list of associations by port creating a map.
groupByPort :: [(Port, a)] -> PortMap [a]
groupByPort = MkPortMap . M.fromListWith (++) . map assoc
  where assoc (p, v) = (fromEnum p, [v])

-- | Convert a map to a sorted list of port associations.
toPortAssocs :: PortMap a -> [(Port, a)]
toPortAssocs (MkPortMap m) = map assoc $ M.assocs m
  where assoc (p, v) = (toEnum p, v)

-- | 
findByPort :: PortMap a -> Port -> a
findByPort (MkPortMap m) p = m M.! (fromEnum p)

type PortsCoverage a = PortMap [Outbound a]

type PortsAdjacency a = PortMap [(Port, a, a, Distance)]

-- | Predicate for competitive itinerary according to direct vs. indirect distances.
competitive_itinerary :: (NormedSpace a) => a -> Distance -> [Move a] -> Bool
competitive_itinerary coord0 dist0 moves = all competitive steps
  where competitive (indirect, direct) = (indirect * ratio) < direct
        ratio = 0.6
        steps = scanl compose (dist0, dist0) moves
        compose (indirect, _) (Move dist coord) = (indirect', direct)
          where indirect' = indirect + dist
                direct = distance coord0 coord

-- | Extend the coverage by one more stop.
extend_coverage :: (NormedSpace a) => PortsAdjacency a -> PortsCoverage a -> PortsCoverage a
extend_coverage adj cov = groupByPort outbounds
  where outbounds = concatMap extend $ zip (toPortAssocs cov) (toPortAssocs adj)
        extend ((port, covs), (port', adjs)) | port == port' = do
          (org, coord_org, coord_port, distance) <- adjs
          Outbound stops dst moves <- covs
          guard $ competitive_itinerary coord_org distance moves
          let stops' = port : stops
              moves' = (Move distance coord_port) : moves
          return (org, Outbound stops' dst moves')

-- | Direct ports coverage from adjacency list.
direct_coverage :: (NormedSpace a) => PortsAdjacency a -> PortsCoverage a
direct_coverage adj = groupByPort $ concatMap adj_to_cov (toPortAssocs adj)
  where adj_to_cov (port, adjs) = do
          (org, _, coord_dst, distance) <- adjs
          let move = Move distance coord_dst
          return (org, Outbound [] port [move])

-- | List of all coverages in path length order.
coverages :: (NormedSpace a) => PortsAdjacency a -> [PortsCoverage a]
coverages adj = iterate (extend_coverage adj) (direct_coverage adj)

--

newtype GeoCoord = GeoCoord (Double, Double) deriving Show

orthodromic_distance :: GeoCoord -> GeoCoord -> Distance
orthodromic_distance _ _ = 1.0

instance NormedSpace GeoCoord where
  distance = orthodromic_distance

type PortsInfo = PortMap GeoCoord

loadPorts :: String -> IO PortsInfo
loadPorts f = return . fromPortAssocs . map parse . drop 1 . T.lines =<< T.readFile f
  where parse row = (port, GeoCoord (lat, lon))
          where col = V.fromList $ T.split (=='^') row
                port = fromJust . toPort . T.encodeUtf8 $ col V.! 0
                lat = read . T.unpack $ col V.! 7
                lon = read . T.unpack $ col V.! 8

-- | Ports adjacency in geographic coordinates
adjacency :: PortsInfo -> [OnD] -> PortsAdjacency GeoCoord
adjacency ports onds = groupByPort $ map make_edge onds
  where make_edge (org, dst) = (dst, (org, coord_org, coord_dst, dist))
          where dist = distance coord_org coord_dst
                coord_org = findByPort ports org
                coord_dst = findByPort ports dst

-- 

main = do
  ports <- loadPorts "ports.csv"
  let ond = (fromJust $ toPort "NCE", fromJust $ toPort "CDG")
      adj = adjacency ports [ond]
      cov = take 3 $ coverages adj
  putStrLn $ show cov

