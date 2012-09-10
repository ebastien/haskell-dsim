module Route (
      PortMap
    , Distance
    , MetricSpace
    , distance
    , Edge(..)
    , PortAdjacencies
    , PortCoverages
    , coverages
    , coveredPaths
    , coveredOnDs
    , ondPaths
    ) where
    
import Control.Monad (foldM, mzero)
import Data.Maybe (mapMaybe)
import qualified EnumMap as M

import Types (Port, Path, OnD)

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

-- | Coverages by origin port.
type PortCoverages e = PortMap (PortMap [Itinerary e])

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
type PortAdjacencies e = PortMap [Edge e]

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
extendedCoverage :: (MetricSpace e) => PortAdjacencies e
                                    -> [PortCoverages e]
                                    -> [PortCoverages e]
extendedCoverage _ [] = error "extendedCoverage on empty coverages list"
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
        filterDist alts dst = filter isShort
          where isShort (Itinerary d _ _) = case alternatives of
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
directCoverage :: (MetricSpace e) => PortAdjacencies e -> PortCoverages e
directCoverage adj = fmap M.group . M.group $ do
  (portB, adjs) <- M.toList adj
  Edge portA _ elemB distAB <- adjs
  return (portA, (portB, Itinerary distAB [] [Step distAB elemB]))

-- | List of all shortest coverages in path length order.
coverages :: (MetricSpace e) => PortAdjacencies e -> [PortCoverages e]
coverages adj = map head $ iterate (extendedCoverage adj) [directCoverage adj]

-- | List of all paths from a coverage.
coveredPaths :: (MetricSpace e) => PortCoverages e -> [Path]
coveredPaths cov = [ org : path ++ [dst] | (org, alts) <- M.toList cov,
                                           (dst, itis) <- M.toList alts,
                                           Itinerary _ path _ <- itis ]

-- | List of all covered OnDs.
coveredOnDs :: (MetricSpace e) => PortCoverages e -> [OnD]
coveredOnDs cov = [ (org, dst) | (org, alts) <- M.toList cov, dst <- M.keys alts]

-- | List of paths for an origin and a destination.
ondPaths :: (MetricSpace e) => OnD -> PortCoverages e -> Maybe [Path]
ondPaths (a,b) cov = case M.lookup a cov >>= M.lookup b of
                       Just itis -> Just $ [ a : path ++ [b]
                                           | Itinerary _ path _ <- itis ]
                       Nothing   -> Nothing
