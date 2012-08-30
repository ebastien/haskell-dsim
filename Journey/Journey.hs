{-# LANGUAGE OverloadedStrings #-}

module Journey (
      test
    , loadPorts
    , adjacency
    , coverages
    , coveragePaths
    ) where
    
import Control.Monad (guard, join, foldM, mzero)
import Data.Maybe (fromJust, isJust, mapMaybe)
import Control.Arrow ((***))
import Data.List (groupBy, sortBy)
import Data.Function (on)
import Data.Traversable (traverse)

import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Text.Encoding as T
import qualified Data.Vector as V

import Types
import qualified EnumMap as M

-- | A sorted collection of port associations.
type PortMap a = M.EnumMap Port a

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

-- | Alternative itineraries by destination port.
type Alternatives e = PortMap [Itinerary e]

-- | Coverages by origin port.
type PortsCoverage e = PortMap (Alternatives e)

-- | An itinerary.
data Itinerary e = Itinerary { iDist  :: Distance -- total indirect distance
                             , iPath  :: Path     -- via ports
                             , iSteps :: [Step e] -- list of steps
                             } deriving (Show)

-- | A step in the graph of ports.
data Step e = Step { sDist :: Distance -- distance
                   , sTo   :: e        -- to element
                   } deriving (Show)

-- | A graph of ports organized as adjacency by destination.
type PortsAdjacency e = PortMap [Edge e]

-- | An edge of the graph of ports.
data Edge e = Edge
    Port      -- origin port
    e         -- origin element
    e         -- destination element
    Distance  -- distance between origin and destination
    deriving (Show)

-- | Competitive distance according to direct vs. indirect distances.
competitiveDistance :: (MetricSpace e) => e -> Distance -> [Step e] -> Maybe Distance
competitiveDistance elemA distAB steps = fmap fst $ foldM detour (distAB, distAB) steps
  where detour (indirectAN, _) (Step distNM elemM)
          | indirectAM < ratio * directAM = Just (indirectAM, directAM)
          | otherwise                     = Nothing
          where directAM = distance elemA elemM
                indirectAM = indirectAN + distNM
                ratio = 1.5

-- | Extend the coverage by one more step.
extendedCoverage :: (MetricSpace e) => PortsAdjacency e
                                    -> [PortsCoverage e]
                                    -> [PortsCoverage e]
extendedCoverage adj covs@(cov:_) = flip (:) covs $ groupOrg $ do
    (portB, alts, adjs) <- zjoin (M.toList cov) (M.toList adj)
    Edge portA elemA elemB distAB <- adjs
    (dest, itis) <- M.toList alts
    Itinerary _ path steps <- itis
    case competitiveDistance elemA distAB steps of
      Nothing    -> mzero
      Just distT -> return (portA, (dest, Itinerary distT path' steps'))
        where path' = portB : path
              steps' = (Step distAB elemB) : steps
  where groupOrg = M.mapWithKey groupDst . M.group
        groupDst org = M.filter (not . null)
                     . M.mapWithKey (filterDist $ map (M.find org) covs)
                     . M.group
        filterDist alts dst = takeWhile short . sortBy (compare `on` iDist)
          where short (Itinerary d _ _) = case alternatives of
                    [] -> True
                    xs -> d <= (minimum $ map (iDist . head) xs) * ratio
                alternatives = mapMaybe (M.lookup dst) alts
                ratio = 1.1

-- | Join two lists of pairs sorted by the first element.
zjoin :: (Ord a) => [(a,b)] -> [(a,c)] -> [(a,b,c)]
zjoin [] _ = []
zjoin _ [] = []
zjoin x@((xa,xb):xs) y@((ya,yb):ys) | xa > ya   = zjoin x ys
                                    | xa < ya   = zjoin xs y
                                    | otherwise = (xa,xb,yb) : zjoin xs ys

-- | Direct ports coverage from adjacency list.
directCoverage :: (MetricSpace e) => PortsAdjacency e -> PortsCoverage e
directCoverage adj = fmap M.group . M.group $ do
  (portB, adjs) <- M.toList adj
  Edge portA _ elemB distAB <- adjs
  return (portA, (portB, Itinerary distAB [] [Step distAB elemB]))

-- | List of all shortest coverages in path length order.
coverages :: (MetricSpace e) => PortsAdjacency e -> [PortsCoverage e]
coverages adj = map head $ iterate (extendedCoverage adj) [directCoverage adj]

-- | List of paths from coverage.
coveragePaths :: (MetricSpace e) => PortsCoverage e -> [Path]
coveragePaths cov = [ org : path ++ [dst] | (org, alts) <- M.toList cov,
                                            (dst, itis) <- M.toList alts,
                                             Itinerary _ path _ <- itis ]

{-
  Geographic coordinates space
-}

newtype GeoCoord = GeoCoord (Double, Double) deriving (Show)

-- | The orthodromic distance between two geographic coordinates.
orthodromicDistance :: GeoCoord -> GeoCoord -> Distance
orthodromicDistance (GeoCoord (latA, lonA)) (GeoCoord (latB, lonB)) = dist
  where dist = 2.0 * radius * (asin . sqrt $ sin2_dLat + cos2_lat * sin2_dLon)
        sin2_dLat = (^(2::Int)) . sin $ (latB - latA) / 2.0
        sin2_dLon = (^(2::Int)) . sin $ (lonB - lonA) / 2.0
        cos2_lat = cos latA * cos latB
        radius = 6367.0

instance MetricSpace GeoCoord where
  distance = orthodromicDistance

-- | Geographic coordinates of ports.
type PortsInfo = PortMap GeoCoord

-- | Load ports information from a file.
loadPorts :: String -> IO PortsInfo
loadPorts f = return . M.fromList . map parse . drop 1 . T.lines =<< T.readFile f
  where radian d = d * pi / 180.0
        parse row = (port, GeoCoord (radian lat, radian lon))
          where port = fromJust . toPort . T.encodeUtf8 $ col V.! 0
                lat = read . T.unpack $ col V.! 7
                lon = read . T.unpack $ col V.! 8
                col = V.fromList $ T.split (=='^') row

-- | Ports adjacency in geographic coordinates.
adjacency :: PortsInfo -> [OnD] -> PortsAdjacency GeoCoord
adjacency ports = M.group . map edge
  where edge (org, dst) = (dst, Edge org coord_org coord_dst dist)
          where dist = distance coord_org coord_dst
                coord_org = M.find org ports
                coord_dst = M.find dst ports

test :: IO ()
test = do
  ports <- loadPorts "ports.csv"
  let adj = adjacency ports $ map (join (***) (fromJust . toPort)) onds
      cov = take 3 $ coverages adj
  mapM_ (putStrLn . show) cov
  where onds = [("NCE", "CDG")
               ,("CDG", "FRA")
               ,("FRA", "JFK")
               ,("CDG", "JFK")] 
