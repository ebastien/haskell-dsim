{-# LANGUAGE OverloadedStrings, NoMonomorphismRestriction #-}

import qualified Filesystem.Path.CurrentOS as FP

import Criterion.Main

import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy as LB

import Data.Attoparsec.Types (Parser)
import qualified Data.Attoparsec.ByteString.Char8 as P
import qualified Data.Attoparsec.ByteString.Lazy as LP

import Data.Functor ((<$>))
import Control.Applicative (some, many, (<*>), (<*))

import Data.Maybe (fromMaybe)

type Designator = ByteString

data Header = Header deriving Show

data Carrier = Carrier { carAirline :: Designator 
                       } deriving Show

data Leg = Leg { lgSuffix :: Char
               , lgAirline :: Designator
               , lgNumber :: Int
               } deriving Show

data Segment = Segment deriving Show

data LegGroup = LegGroup { lgLeg :: Leg
                         , lgSegments :: [Segment] } deriving Show

data CarrierGroup = CarrierGroup { cgCarrier :: Carrier
                                 , cgLegs :: [LegGroup] } deriving Show

data Ssim = Ssim { ssimHeader :: Header
                 , ssimCarriers :: [CarrierGroup] } deriving Show

headerP  = P.char '1' >> P.take 199 >> (some P.endOfLine) >> return Header

carrierP = do
  P.char '2'
  P.satisfy (\c -> c == 'U' || c == 'L')
  airline <- airlineP
  P.take 195
  some P.endOfLine
  return (Carrier airline)

legP = do
  P.char '3'
  suffix <- P.anyChar
  airline <- airlineP
  fnum <- fnumP
  iviL <- decimalP 2
  lsn <- decimalP 2
  P.anyChar
  bdate <- P.take 7
  edate <- P.take 7
  dow <- P.take 7
  P.anyChar
  bpoint <- P.take 3
  P.take 4
  dtime <- P.take 4
  dutc <- P.take 5
  P.take 2
  opoint <- P.take 3
  atime <- P.take 4
  P.take 4
  autc <- P.take 5
  P.take (2+3+20+5+10+9+2+6)
  iviH <- decimalP 1
  P.take (3+3+3+9+1+1+1+11+1+11+20)
  dvar <- P.anyChar
  avar <- P.anyChar
  P.take 6
  some P.endOfLine
  let pnum = iviL + 100 * iviH in
    return (Leg suffix airline fnum)

-- | Parser for fixed length decimal numbers.
decimalP :: Int -> Parser ByteString Int
decimalP s = do 
  r <- (B8.readInt . B8.dropWhile (== ' ')) <$> P.take s
  fromMaybe (fail "Decimal parsing failed") $ (return . fst) <$> r


dateP :: Parser ByteString Int

-- | Parser for flight numbers.
fnumP = decimalP 4

-- | Parser for airline designators.
airlineP = P.take 3

segmentP = P.char '4' >> P.take 199 >> (some P.endOfLine) >> return Segment

trailerP = P.char '5' >> P.take 199 >> (some P.endOfLine) >> return ()

legGroupP = LegGroup <$> legP <*> (many segmentP)

carrierGroupP = CarrierGroup <$> carrierP <*> (many legGroupP)

ssimP = Ssim <$> headerP <*> (many carrierGroupP) <* trailerP

resultLazy :: FP.FilePath -> IO (LP.Result Ssim)
resultLazy s = do
  c <- LB.readFile $ FP.encodeString s
  return $ LP.parse ssimP c

main = defaultMain [
  bench "lazy" $ whnfIO $ resultLazy input
  ]
  where input = "oag.ssim7.sample"
