module Connection (
      fromSegments
    , toOnDs
    , connections
    , OnDSegments
    ) where

import Data.Maybe (mapMaybe)
import Data.Monoid (mconcat, First(..), getFirst)
import Control.Monad (mzero)

import Data.Time.Calendar (Day, addDays, diffDays)
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
lookupDate s@(l:_) depDate = if withinPeriod (lpPeriod $ fst l) depDate
                               then Just $ MkSegmentDate s depDate depTime arrDate arrTime
                               else Nothing
  where depTime = lpDepartureTime . fst $ l
        arrDate = addDays (fromIntegral $ lpArrivalDateVariation lastLeg) depDate
        arrTime = lpArrivalTime lastLeg
        lastLeg = fst $ last s

-- | Convert a path into a list of OnDs.
toSteps :: Path -> [OnD]
toSteps path = zip path $ tail path

-- | Feasible connections on a given day.
connections :: OnDSegments -> Day -> Path -> [[SegmentDate]]
connections onds d0 = map (($[]) . fst) . foldl combine init . toSteps
  where init = [(id, ( d0                         -- departure date
                     , secondsToDiffTime 0        -- departure time
                     , secondsToDiffTime 0        -- minimum connecting time
                     , secondsToDiffTime 24*60*60 -- maximum connecting time
               ))]
        combine parts (a, b) = do
            (done, (arrDate, arrTime, cmin, cmax)) <- parts
            s <- M.find (MkPOnD a b) onds
            case connect arrDate arrTime cmin cmax s of
              Nothing -> mzero
              Just o  -> let cmin' = secondsToDiffTime 30*60
                             cmax' = secondsToDiffTime 6*60*60
                             limits = ( sdArrivalDate o
                                      , sdArrivalTime o
                                      , cmin', cmax') in
                         return $ (done . (o:), limits)

-- | Connect an outbound segment with given time constraints.
connect :: Day
        -> ScheduleTime
        -> TimeDuration
        -> TimeDuration
        -> SegmentPeriod
        -> Maybe SegmentDate
connect d0 t0 cmin cmax s = getFirst . mconcat $ map (First . lookupDate s) depDates
  where depDates = takeWhile shortEnough $ dropWhile tooShort [d0..]
        shortEnough d = wait d < cmax
        tooShort d = wait d < cmin
        wait d = t1 - t0 + secondsToDiffTime (diffDays d d0 * 86400)
        t1 = lpDepartureTime . fst $ head s
