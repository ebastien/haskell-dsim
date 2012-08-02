{-# LANGUAGE OverloadedStrings, NoMonomorphismRestriction #-}

import qualified Filesystem.Path.CurrentOS as FP

import Criterion.Main

import Data.ByteString (ByteString)
import qualified Data.ByteString as B

import Data.Attoparsec.Types (Parser)
import qualified Data.Attoparsec.ByteString.Char8 as P

import qualified Data.ByteString.Lazy as LB
import qualified Data.Attoparsec.ByteString.Lazy as LP

import Data.Functor ((<$>))
import Control.Applicative (some, many, (<*>), (<*))

type Designator = ByteString

data Header = Header deriving Show

data Carrier = Carrier { carAirline :: Designator } deriving Show

data Leg = Leg deriving Show

data Segment = Segment deriving Show

data LegGroup = LegGroup { lgLeg :: Leg
                         , lgSegments :: [Segment] } deriving Show

data CarrierGroup = CarrierGroup { cgCarrier :: Carrier
                                 , cgLegs :: [LegGroup] } deriving Show

data Ssim = Ssim { ssimHeader :: Header
                 , ssimCarriers :: [CarrierGroup] } deriving Show

headerParser  = P.char '1' >> P.take 199 >> (some P.endOfLine) >> return Header

carrierParser = do
  P.char '2'
  P.satisfy (\c -> c == 'U' || c == 'L')
  ad <- P.take 3
  P.take 195
  some P.endOfLine
  return (Carrier ad)

legParser     = P.char '3' >> P.take 199 >> (some P.endOfLine) >> return Leg

segmentParser = P.char '4' >> P.take 199 >> (some P.endOfLine) >> return Segment

trailerParser = P.char '5' >> P.take 199 >> (some P.endOfLine) >> return ()

legGroupParser = LegGroup <$> legParser <*> (many segmentParser)

carrierGroupParser = CarrierGroup <$> carrierParser <*> (many legGroupParser)

ssimParser = Ssim <$> headerParser <*> (many carrierGroupParser) <* trailerParser

resultLazy :: FP.FilePath -> IO (LP.Result Ssim)
resultLazy s = do
  c <- LB.readFile $ FP.encodeString s
  return $ LP.parse ssimParser c

main = defaultMain [
  bench "lazy" $ whnfIO $ resultLazy input
  ]
  where input = "oag.ssim7.sample"
