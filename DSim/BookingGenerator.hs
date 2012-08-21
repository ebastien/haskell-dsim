{-# LANGUAGE FlexibleContexts, PackageImports #-}

import qualified Data.Vector as V

import Data.List (mapAccumL)
import Data.Maybe (fromMaybe)

import Data.Random.Source.PureMT (pureMT)
import Data.Random.Source.Std (StdRandom(StdRandom))
import Data.Random.Distribution (Distribution, rvar)
import Data.Random.Distribution.Uniform (Uniform, uniform)
import Data.Random.Distribution.Categorical (fromWeightedList)
import Data.Random.RVar (RVar, runRVar)
import Control.Monad.State.Lazy (evalState)
import Control.Monad (replicateM)

import "data-accessor" Data.Accessor
import Graphics.Rendering.Chart
import Graphics.Rendering.Chart.Gtk

-- A discrete range of indexed values
data DiscreteRange a = DiscreteRange (Int, Int) [(Int, a)] deriving Show

-- Distribute a big enough number of events according to a probability mass function
-- The approximation may be wrong if the number of events is small compared to the probabiliy masses
ldistrib :: (RealFrac a, Integral b) => DiscreteRange a -> b -> DiscreteRange b
ldistrib (DiscreteRange bounds values) n = DiscreteRange bounds (snd $ mapAccumL step (n, 0) values)
    where step (m, c) (i, p) = ((m - k, c + p), (i, k))
              where k = floor (p * (fromIntegral m) / (1 - c))

-- Interpolate a discrete cummulative distribution
intercdf :: (Fractional a) => DiscreteRange a -> DiscreteRange a
intercdf (DiscreteRange bounds values) = DiscreteRange bounds (concat $ interp values)
    where interp (y:[]) = [[y]]
          interp ((i, pi):y@(j, pj):xs) = rg:(interp (y:xs))
              where dp = (pj - pi) / (fromIntegral (j - i))
                    rg = [(k, pi + (fromIntegral (k - i)) * dp) | k <- [i..(j - 1)]]

-- Derive a discrete cummulative distribution
derivcdf :: (Fractional a) => DiscreteRange a -> DiscreteRange a
derivcdf (DiscreteRange bounds values) = DiscreteRange bounds ((head values):(derive values))
    where derive (_:[]) = []
          derive ((i, pi):y@(j, pj):xs) = (j, (pj - pi)):(derive (y:xs))

-- Uniform discrete probability masses over a range
uniformpmf :: (Fractional a) => Int -> Int -> DiscreteRange a
uniformpmf m n = DiscreteRange (m, n) [ (i, pi) | i <- [m..n] ]
    where pi = 1.0 / (fromIntegral (n - m + 1))

-- An array with index offset
data OffsetArray a = OffsetArray { firstIndex :: Int, arrayValues :: V.Vector a } deriving Show

-- Last index of an array with offset
lastIndex :: OffsetArray a -> Int
lastIndex (OffsetArray o v) = o + (V.length v) - 1

-- Bounds of an array with offset
indexBounds :: OffsetArray a -> (Int, Int)
indexBounds a = (firstIndex a, lastIndex a)

-- Safe lookup with default value
safeAt :: a -> OffsetArray a -> Int -> a
safeAt q a n = fromMaybe q ((arrayValues a) V.!? (n - (firstIndex a)))

-- Conversion from discrete range to array with offset
fromDiscreteRange :: (Num b) => DiscreteRange b -> OffsetArray b
fromDiscreteRange (DiscreteRange (m, n) values) = OffsetArray m (V.accum (+) vzero translated)
    where length = n - m + 1
          vzero = V.replicate length 0
          translated = map (\(i, x) -> (i - m, x)) values

-- Convolution of two arrays
aconv :: (Num a) => OffsetArray a -> OffsetArray a -> OffsetArray a
aconv a b = OffsetArray first (V.generate length step)
    where fa = firstIndex a ; fb = firstIndex b
          la = lastIndex a  ; lb = lastIndex b
          first = fa + fb   ; last = la + lb    ; length = last - first + 1
          step i = sum [ (safeAt 0 a k) * (safeAt 0 b (n - k)) | k <- [kmin..kmax] ]
              where n = i + first
                    kmin = max fa (n - lb)
                    kmax = min la (n - fb)

-- Crop an array
crop :: (Int, Int) -> OffsetArray a -> OffsetArray a
crop (m, n) a = OffsetArray m' (V.slice (m' - i) (n' - m') (arrayValues a))
    where i = firstIndex a
          j = lastIndex a
          m' = min (max m i) j
          n' = min (max n i) j

-- Histogram of a steam of categorical events identified by Int values
histogram :: (Int, Int) -> [Int] -> OffsetArray Int
histogram (m, n) events = OffsetArray m (V.accum (+) vzero indices)
    where length = n - m + 1
          vzero = V.replicate length 0
          indices = map (\i -> (i-m, 1)) events

-- Convert a discrete probability mass distribution to a categorical random variable
-- The random variable is normalized even if the probabilities do not sum up to 1
fromOffsetArray :: (Distribution Uniform a,
                    Ord a, Fractional a) => OffsetArray a -> RVar Int
fromOffsetArray a = rvar . fromWeightedList $ V.ifoldl step [] (arrayValues a)
    where offset = firstIndex a
          step xs i value = (value, i + offset):xs

-- Example

-- Partial advance purchase cummulative distribution
advp_partial = DiscreteRange (-330, -1) [(-330, 0), (-40, 0.2), (-20, 0.6), (-1, 1.0)] :: DiscreteRange Double

-- Interpolated advance purchase cummulative distribution
advp_cdf = intercdf advp_partial

-- Distribution of advance purchase probabilities
advp_pmf = fromDiscreteRange $ derivcdf advp_cdf

-- Uniform distribution of trip probabilities over a year
trip_range = (0, 364)
trip_pmf = fromDiscreteRange $ uncurry uniformpmf trip_range

-- Distribution of booking probabilities
bkng_pmf = crop trip_range (aconv trip_pmf advp_pmf)

-- a categorical random variable
bkng_rvar = fromOffsetArray bkng_pmf

-- a random variable for 10K bookings
all_bkng_rvar = replicateM 10000 bkng_rvar

-- the random action
all_bkng_act = runRVar all_bkng_rvar StdRandom

-- the evaluation of the action from the given intial state
all_bkng_draw = evalState all_bkng_act (pureMT 0)

-- the counters of bookings over a range of days
bkng_hist = histogram (indexBounds bkng_pmf) all_bkng_draw

-- Plotting
bkng_chart_values = zip [firstIndex bkng_hist..] (map (:[]) $ V.toList $ arrayValues bkng_hist)

bars = plot_bars_titles ^= ["booking requests"]
     $ plot_bars_style ^= BarsClustered
     $ plot_bars_values ^= bkng_chart_values
     $ defaultPlotBars

layout = layout1_title ^= "Daily booking requests"
       $ layout1_plots ^= [ Left (plotBars bars) ]
       $ defaultLayout1 :: Layout1 Int Int

rendering = toRenderable layout

main = do
      putStrLn $ show bkng_hist
--    renderableToSVGFile rendering 800 600 "plot.svg"

