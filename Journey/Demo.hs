module Demo (main) where

import Data.Functor ((<$>))
import Text.Printf (printf)

import Ssim (readSsimFile, ssimOnDs)
import Journey (
    loadPorts
  , adjacency
  , coverages
  , coveragePaths
  )

main :: IO ()
main = do
  ports <- loadPorts "ports.csv"
  onds <- ssimOnDs <$> readSsimFile "../oag/oag.ssim7.sample"
  printf "%d OnDs loaded from SSIM file.\n" $ length onds
  let adj = adjacency ports onds
      covs = take 8 $ coverages adj
  mapM_ (print . length . coveragePaths) covs
  print . coveragePaths $ last covs
