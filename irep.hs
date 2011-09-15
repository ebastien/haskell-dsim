import Data.Ratio
import Data.List
import Data.Array.IArray
import Data.Tuple
import qualified Data.Vector as V
import Data.Maybe (fromMaybe)
import Data.Time.Calendar

import Data.Random.Source.PureMT (pureMT)
import Data.Random.Source.Std
import Data.Random.Distribution.Uniform (uniform)
import Data.Random.Distribution.Categorical (categorical)
import Data.Random.RVar (RVar, runRVar)
import Control.Monad.State.Lazy (evalState)
import Control.Monad (replicateM)

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

-- Safe lookup with default value
safeAt :: a -> OffsetArray a -> Int -> a
safeAt q a n = fromMaybe q ((arrayValues a) V.!? (n - (firstIndex a)))

-- Conversion from discrete range
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

--

partial_cdf = DiscreteRange (-330, -1) [(-330, 0), (-40, 0.2), (-20, 0.6), (-1, 1.0)] :: DiscreteRange Double

cdf = intercdf partial_cdf
pmf = derivcdf cdf
ldis = ldistrib pmf 10000

-- Work in progress:
-- Randomized distribution of events according to a probability mass function

-- Convert a discrete probability mass distribution to a categorical random variable
pmf2cat (DiscreteRange _ values) = categorical (uncurry zip (swap . unzip $ values))

-- a categorical random variable
c = pmf2cat pmf

-- a random variable for 10K categorical draws
v = replicateM 10000 c

-- the random action
a = runRVar v StdRandom

-- the evaluation of the action from the given intial state
x = evalState a (pureMT 0)

hist :: (Ix a, Num b) => (a, a) -> [a] -> Array a b
hist bnds is = accumArray (+) 0 bnds [(i, 1) | i <- is]

-- the histogram
h = hist (-330, -1) x

--

