{-# LANGUAGE OverloadedStrings #-}

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

import qualified EnumMap as M
import Data.Maybe (fromJust)
import Types (toPort)
import Data.ByteString.Char8 (pack)

main :: IO ()
main = do
  [portsFile, ssimFile, org, dst] <- getArgs
  ports <- loadPorts portsFile
  onds <- ssimOnDs <$> readSsimFile ssimFile
  printf "%d OnDs loaded from SSIM file\n" $ length onds :: IO ()
  let adj = adjacency ports onds
      covs = take 3 $ coverages adj
  mapM_ (printf "%d paths found\n" . length . coveragePaths) covs
  let itis = M.lookup (fromJust . toPort $ pack dst)
           . M.find (fromJust . toPort $ pack org)
  mapM_ (print . itis) covs
