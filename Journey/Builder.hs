{-# LANGUAGE OverloadedStrings #-}

module Builder (
    buildAll
    ) where

import Data.List (sort, group, intersperse)
import Data.Monoid (mconcat, mempty)
import Data.Foldable (foldMap)
import Data.Text.Lazy.Builder (Builder, fromString, singleton)
import Data.Text.Format (build, left, Shown(..))
import Data.Time.LocalTime (TimeOfDay(..), timeToTimeOfDay)
import Data.Time.Calendar (Day, toGregorian)

import Types ( Port, OnD, Path, ScheduleTime
             , Flight(..), LegPeriod(..), SegmentDate(..) )
import Route ( coveredOnDs, ondPaths, MetricSpace, PortCoverages )
import Connection ( connections, OnDSegments )

-- | Build a representation of all itineraries departing on a given day.
buildAll :: (MetricSpace e) => OnDSegments
                            -> [PortCoverages e]
                            -> Day
                            -> Builder
buildAll segs covs date = foldMap (buildForOnD segs covs date) onds
  where onds = map head . group . sort $ concatMap coveredOnDs covs

buildForOnD :: (MetricSpace e) => OnDSegments
                               -> [PortCoverages e]
                               -> Day
                               -> OnD
                               -> Builder
buildForOnD segs covs date ond = foldMap f covs
  where f cov = case ondPaths ond cov of
                  Just paths -> foldMap (buildForPath date segs ond) paths
                  Nothing    -> mempty

buildForPath :: Day -> OnDSegments -> OnD -> Path -> Builder
buildForPath date segs ond path = foldMap f $ connections segs date path
  where f c = mconcat [ buildOnD ond   , singleton '\t'
                      , buildPath path , singleton '\t'
                      , buildCnx c
                      , singleton '\n']

buildOnD :: OnD -> Builder
buildOnD (org,dst) = buildPath [org,dst]

buildPath :: Path -> Builder
buildPath = mconcat . intersperse (singleton '-') . map buildPort

buildPort :: Port -> Builder
buildPort = fromString . show

buildCnx :: [SegmentDate] -> Builder
buildCnx = mconcat . intersperse (singleton ';') . map buildSeg

buildSeg :: SegmentDate -> Builder
buildSeg s = mconcat . intersperse (singleton ' ')
           $ [ buildFlight f
             , buildPort $ lpBoard l
             , buildPort $ lpOff l
             , buildDate $ sdDepartureDate s
             , buildTime $ sdDepartureTime s
             , buildDate $ sdArrivalDate s
             , buildTime $ sdArrivalTime s
             ]
  where l = fst . head $ sdSegment s
        f = lpFlight l

buildFlight :: Flight -> Builder
buildFlight f = build "{} {}" (Shown $ fAirline f, left 5 ' ' $ fNumber f)

buildDate :: Day -> Builder
buildDate date = mconcat [pad 4 y, pad 2 m, pad 2 d]
  where (y,m,d) = toGregorian date
        pad n = left n '0'

buildTime :: ScheduleTime -> Builder
buildTime t = build "{}:{}" [pad h, pad m]
  where TimeOfDay h m _ = timeToTimeOfDay t
        pad = left 2 '0'
