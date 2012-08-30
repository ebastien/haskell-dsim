module Main (main) where

import Data.Functor ((<$>))
import Text.Printf (printf)
import System.Environment (getArgs)

import Ssim (readSsimFile, ssimOnDs)
import Journey (
    loadPorts
  , adjacency
  , coverages
  , coveragePaths
  )

main :: IO ()
main = do
  [portsFile, ssimFile] <- getArgs
  ports <- loadPorts portsFile
  onds <- ssimOnDs <$> readSsimFile ssimFile
  printf "%d OnDs loaded from SSIM file\n" $ length onds :: IO ()
  let adj = adjacency ports onds
      covs = take 8 $ coverages adj
  mapM_ (printf "%d paths found\n" . length . coveragePaths) covs
  print . coveragePaths $ last covs
