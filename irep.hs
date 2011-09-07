import Data.Ratio

-- Distribute a big enough number of events according to a probability mass function
-- The approximation may be wrong if the number of events is small compared to the probabiliy masses
repart :: (Integral a, Integral b) => [Ratio a] -> b -> [b]
repart pmf n = snd . unzip . tail $ scanl step ((n, 0), 0) pmf
    where step ((m, c), _) p = ((m - k, c + p), k)
              where k = floor (p * (fromIntegral m) / (1 - c))

-- Interpolate a discrete distribution from a list of samples
intercdf :: (Integral a, Fractional b) => [(a, b)] -> [(a, b)]
intercdf smp = concat $ interp smp
  where interp (y:[]) = [[y]]
        interp ((i, pi):y@(j, pj):xs) = [(k, pi + (fromIntegral (k-i)) * dp) | k <- [i..(j-1)]]:(interp (y:xs))
            where dp = (pj-pi) / (fromIntegral (j-i))

test_pmf = [0.15, 0.05, 0.05, 0.05, 0.2, 0.0, 0.2, 0.05, 0.05, 0.05, 0.15] :: [Ratio Int]

cdf_sample = [(-330, 0), (-40, 0.2), (-20, 0.6), (-1, 1.0)]

main = do
  mapM_ print $ repart test_pmf 349
  mapM_ print $ intercdf cdf_sample
