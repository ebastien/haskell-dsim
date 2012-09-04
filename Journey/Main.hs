{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Data.Functor ((<$>))
import Text.Printf (printf)
import System.Environment (getArgs)
import Data.Maybe (fromJust)
import Types (toPort, toDate)
import Data.ByteString.Char8 (pack)
import Data.List (intercalate, nub)
import Data.Time.LocalTime (TimeOfDay(..), timeToTimeOfDay)
import Data.Time.Calendar (Day, toGregorian)

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

printAll :: (MetricSpace e) => Day -> OnDSegments -> [PortCoverages e] -> IO ()
printAll date segs covs = mapM_ (printOnD date segs covs) onds
  where onds = nub $ concatMap coveredOnDs covs

printOnD :: (MetricSpace e) => Day -> OnDSegments -> [PortCoverages e] -> OnD ->  IO ()
printOnD date segs covs ond = mapM_ loop covs
  where loop cov = case ondPaths ond cov of
                     Just ps -> mapM_ (printPath date segs) ps
                     Nothing -> return ()

printPath :: Day -> OnDSegments -> Path -> IO ()
printPath date segs path = mapM_ printCnx $ connections date segs path

printCnx :: [SegmentDate] -> IO ()
printCnx cnx = putStrLn . intercalate ";" . map fmtSeg $ cnx
  where fmtSeg s = printf "%s %s %s %s %s %s"
                     (fmtFlight f) (show $ lpBoard l) (show $ lpOff l)
                     (fmtDate $ sdDate s)
                     (fmtTime $ lpDepartureTime l)
                     (fmtTime $ lpArrivalTime l) :: String
          where l = fst . head $ sdSegment s
                f = lpFlight l

fmtFlight :: Flight -> String
fmtFlight f = printf "%s%4d" (show $ fAirline f) (fNumber f) :: String

fmtDate :: Day -> String
fmtDate date = printf "%04d%02d%02d" y m d :: String
  where (y,m,d) = toGregorian date

fmtTime :: ScheduleTime -> String
fmtTime t = printf "%02d:%02d" h m :: String
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

  printAll date segments covs
  
  -- printOnD date segments covs ond

