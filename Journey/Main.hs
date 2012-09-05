{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Data.Functor ((<$>))
import Text.Printf (printf)
import System.Environment (getArgs)
import Data.Maybe (fromJust, mapMaybe)
import Types (toPort, toDate)
import Data.ByteString.Char8 (pack)
import Data.List (intercalate, nub)
import Data.Time.LocalTime (TimeOfDay(..), timeToTimeOfDay)
import Data.Time.Calendar (Day, toGregorian)
import Control.Monad (forM_, forM)
import Data.Monoid (mconcat)

import Types (
    OnD
  , Path
  , ScheduleTime
  )
import Ssim (
    readSsimFile
  , ssimSegments
  , Flight(..)
  , LegPeriod(..)
  , SegmentDate(..)
  )
import Journey (
    coverages
  , coveredPaths
  , coveredOnDs
  , ondPaths
  , MetricSpace
  , PortCoverages
  )
import GeoCoord (
    loadReferences
  , assocToCities
  , adjacency
  )
import Connection (
    fromSegments
  , toOnDs
  , connections
  , OnDSegments
  )

fmtAll :: (MetricSpace e) => Day -> OnDSegments -> [PortCoverages e] -> ShowS
fmtAll date segs covs = foldr (.) id $ map (fmtOnD date segs covs) onds
  where onds = nub $ concatMap coveredOnDs covs

fmtOnD :: (MetricSpace e) => Day -> OnDSegments -> [PortCoverages e] -> OnD -> ShowS
fmtOnD date segs covs ond = foldr (.) id $ map format covs
  where format cov = case ondPaths ond cov of
                       Just paths -> foldr (.) id $ map (fmtPath date segs) paths
                       Nothing    -> id

fmtPath :: Day -> OnDSegments -> Path -> ShowS
fmtPath date segs path = foldr (.) id $ map fmtCnx (connections date segs path)

fmtCnx :: [SegmentDate] -> ShowS
fmtCnx cnx = foldr (.) id $ map fmtSeg cnx

fmtSeg :: SegmentDate -> ShowS
fmtSeg s = foldr (.) id $ [ fmtFlight f, shows $ lpBoard l, shows $ lpOff l,
                            fmtDate $ sdDate s,
                            fmtTime $ lpDepartureTime l,
                            fmtTime $ lpArrivalTime l ]
  where l = fst . head $ sdSegment s
        f = lpFlight l

fmtFlight :: Flight -> ShowS
fmtFlight f = (++) $ printf "%s%4d" (show $ fAirline f) (fNumber f)

fmtDate :: Day -> ShowS
fmtDate date = (++) $ printf "%04d%02d%02d" y m d
  where (y,m,d) = toGregorian date

fmtTime :: ScheduleTime -> ShowS
fmtTime t = (++) $ printf "%02d:%02d" h m
  where TimeOfDay h m _ = timeToTimeOfDay t

-- | 
main :: IO ()
main = do
  [refsFile, ssimFile, org, dst, day] <- getArgs
  
  refs <- loadReferences refsFile
  
  segments <- fromSegments . assocToCities refs . ssimSegments <$> readSsimFile ssimFile
  let onds = toOnDs segments
  
  -- printf "%d OnDs loaded from SSIM file\n" $ length onds :: IO ()
  
  let adj = adjacency refs onds
      covs = take 3 $ coverages adj
  
  -- mapM_ (printf "%d paths found\n" . length . coveragePaths) covs
  
  let ond = (fromJust . toPort $ pack org, fromJust . toPort $ pack dst)
      date = fromJust . toDate $ pack day

  -- mapM_ (print . ondPaths ond) covs

  putStr $ fmtAll date segments covs ""
  
  -- printOnD date segments covs ond

