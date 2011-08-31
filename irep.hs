import Data.Ratio

test_pmf = [0.15, 0.05, 0.05, 0.05, 0.2, 0.0, 0.2, 0.05, 0.05, 0.05, 0.15] :: [Ratio Int]

-- Distribute a total number of events according to a probability mass function
repart :: (Integral a, Integral b) => [Ratio a] -> b -> [(b, Ratio a, b)]
repart pmf n = tail $ scanl step (n, 0, 0) pmf
    where step (m, c, _) p = (m - k, c + p, k)
              where k = floor (p * (fromIntegral m) / (1 - c))
