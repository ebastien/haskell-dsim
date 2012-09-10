{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Data.Functor ((<$>))
import Data.Maybe (fromJust)
import Data.Foldable (foldMap)
import Data.ByteString.Char8 (pack)
import qualified Data.Text.Lazy.IO as T
import Data.Text.Lazy.Builder (toLazyText)
import System.Environment (getArgs)

import Ssim (readSsimFile, ssimSegments, toDate)
import Journey (coverages)
import GeoCoord (loadReferences, assocToCities, adjacency)
import Connection (fromSegments, toOnDs)
import Builder (buildAll)

-- | 
main :: IO ()
main = do
  [refsFile, ssimFile, beginDate, endDate] <- getArgs
  refs <- loadReferences refsFile
  segdb <- fromSegments . assocToCities refs . ssimSegments <$> readSsimFile ssimFile

  let covs = take 3 . coverages . adjacency refs $ toOnDs segdb
      dateL = fromJust . toDate $ pack beginDate
      dateH = fromJust . toDate $ pack endDate

  -- let (a,b) = join (***) (fromJust . toPort) ("NYC", "STL")
  -- mapM_ (putStrLn . show) $ M.find (MkPOnD a b) segments
  -- T.putStr . toLazyText . buildForOnD segments covs dateL $ join (***) (fromJust . toPort) ("LON", "DFW")

  T.putStr . toLazyText $ foldMap (buildAll segdb covs) [dateL..dateH]
