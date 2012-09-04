module GeoCoord (
      loadReferences
    , assocToCities
    , adjacency
    ) where

import Control.Monad (join)
import Data.Maybe (fromJust)
import Control.Arrow ((***))
import Data.List (nub)

import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Text.Encoding as T
import qualified Data.Vector as V
import qualified EnumMap as M

import Journey
import Types

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

-- | Make geographic coordinates from a pair of latitude and longitude.
fromDegree :: (Double, Double) -> GeoCoord
fromDegree (lat, lon) = GeoCoord (radian lat, radian lon)
  where radian d = d * pi / 180.0

-- | Geographic reference data.
data Reference = Reference { rCoord :: GeoCoord
                           , rCity  :: Port
                           } deriving (Show)

-- | Port references.
type PortReferences = PortMap Reference

-- | Load ports information from a file.
loadReferences :: String -> IO PortReferences
loadReferences f = return . M.fromList . map parse . drop 1 . T.lines =<< T.readFile f
  where parse row = (port, reference)
          where col = V.fromList $ T.split (=='^') row
                port = fromJust . toPort . T.encodeUtf8 $ col V.! 0
                lat = read . T.unpack $ col V.! 7
                lon = read . T.unpack $ col V.! 8
                city = fromJust . toPort . T.encodeUtf8 $ col V.! 31
                reference = Reference (fromDegree (lat, lon)) city

toCity :: PortReferences -> Port -> Port
toCity refs port = rCity $ M.find port refs

toCities :: PortReferences -> OnD -> OnD
toCities refs = join (***) (toCity refs)

assocToCities :: PortReferences -> [(OnD, a)] -> [(OnD, a)]
assocToCities refs = map assoc
  where assoc (ond, x) = (toCities refs ond, x)

-- | Ports adjacency in geographic coordinates.
adjacency :: PortReferences -> [OnD] -> PortAdjacencies GeoCoord
adjacency refs = M.group . map edge . nub . filter valid
  where valid (org, dst) = org /= dst
        edge (org, dst) = (dst, Edge org coord_org coord_dst dist)
          where dist = distance coord_org coord_dst
                coord_org = rCoord $ M.find org refs
                coord_dst = rCoord $ M.find dst refs
