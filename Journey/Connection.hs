module Connection (
      fromSegments
    , toOnDs
    , connections
    , OnDSegments
    ) where

import Data.Maybe (mapMaybe)
import Data.Monoid (mconcat, First(..), getFirst)

import Data.Time.Calendar (Day, diffDays)
import Data.Time.Clock (secondsToDiffTime)

import qualified EnumMap as M

import Types
import Ssim

import Text.Printf (printf)
import Debug.Trace (trace)

{-------------------------------------------------------------------------------
  Connection building
-------------------------------------------------------------------------------}

-- | A packed OnD.
data POnD = MkPOnD !Port !Port deriving (Show)

instance Enum POnD where
  fromEnum (MkPOnD a b) = (fromEnum a) * 26^3 + (fromEnum b)
  toEnum i = let (a,b) = divMod i (26^3) in MkPOnD (toEnum a) (toEnum b)

-- | A collection of OnD associations.
type OnDMap a = M.EnumMap POnD a

-- | A collection of OnD and segments associations.
type OnDSegments = OnDMap [SegmentPeriod]

-- | Create the collection of segments grouped by OnD.
fromSegments :: [(OnD, SegmentPeriod)] -> OnDSegments
fromSegments = M.group . map packOnd
  where packOnd ((org,dst), segment) = (MkPOnD org dst, segment)

-- | List unique OnDs.
toOnDs :: OnDSegments -> [OnD]
toOnDs = map unpackOnd . M.keys
  where unpackOnd (MkPOnD a b) = (a,b)

-- | Look for a matching date in a period.
lookupDate :: SegmentPeriod -> Day -> Maybe SegmentDate
lookupDate s@(l:_) d = if withinPeriod (lpPeriod $ fst l) d
                         then Just $ MkSegmentDate s d
                         else Nothing

-- | Feasible connections on a given day.
connections :: Day -> OnDSegments -> Path -> [[SegmentDate]]
connections d0 onds = mapMaybe (connectAll d0) . walk [id]
  where walk done (b:[])   = map ($[]) done
        walk done (a:b:ps) = walk done' (b:ps)
          where done' = [ d . (c:) | c <- choices, d <- done ]
                choices = M.find (MkPOnD a b) onds

-- | Connect all segments of a journey.
connectAll :: Day -> [SegmentPeriod] -> Maybe [SegmentDate]
-- connectAll d0 xs | trace (printf "connectAll %s %s" (show d0) (show xs)) False = undefined
connectAll d0 xs = case lookupDate (head xs) d0 of
                     Just s0 -> walk d0 (s0:) xs
                     Nothing -> Nothing
  where walk _ trip (_:[])   = Just $ trip []
        walk d trip (a:b:xs) = case connect d a b of
                                 Just s  -> walk (sdDate s) (trip . (s:)) (b:xs)
                                 Nothing -> Nothing

-- | Connect an inbound segment with an outbound segment
connect :: Day -> SegmentPeriod -> SegmentPeriod -> Maybe SegmentDate
-- connect d a b | trace (printf "connect %s %s %s" (show d) (show a) (show b)) False = undefined
connect d a b = getFirst . mconcat $ map (First . lookupDate b) days
  where days = takeWhile shortEnough $ dropWhile tooShort [d..]
        shortEnough d' = wait d' < secondsToDiffTime 6*60*60
        tooShort d' = wait d' < secondsToDiffTime 30*60
        wait d' = depB - arrA + secondsToDiffTime (diffDays d' d * 86400)
        arrA = lpArrivalTime . fst $ last a
        depB = lpDepartureTime . fst $ head b
