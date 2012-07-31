{-# LANGUAGE OverloadedStrings, NoMonomorphismRestriction #-}

import Data.Conduit
import Data.Conduit.Filesystem (traverse, sourceFile)
import Control.Monad.IO.Class (liftIO)
import System.Environment (getArgs)
import qualified Data.Conduit.Binary as CB
import qualified Data.Conduit.List as CL
import qualified Filesystem.Path.CurrentOS as FP

import Text.Regex.TDFA ((=~))

import Control.Concurrent (threadDelay)

import Debug.Trace (trace)
import Criterion.Main

import Data.ByteString (ByteString)
import qualified Data.ByteString as B

import Data.Attoparsec.Types (Parser)
import qualified Data.Attoparsec.ByteString.Char8 as P
import Data.Attoparsec.Combinator (choice, many1)
import Data.Conduit.Attoparsec (conduitParser)

import qualified Data.ByteString.Lazy as LB
import qualified Data.Attoparsec.ByteString.Lazy as LP

import Data.Maybe (fromMaybe)
import Data.Functor ((<$>))

count :: Monad m => Sink ByteString m Int
count = CB.lines =$ CL.fold (\n _ -> n + 1 ) 0

readAll :: Source (ResourceT IO) ByteString
readAll = sourceArgs
      $= awaitForever sourceDir
      $= CL.filter filePattern
      $= CL.mapM slowly
      $= awaitForever sourceFile
      where sourceArgs = liftIO getArgs >>= mapM_ yield
            sourceDir f = traverse False (FP.decodeString f)
            filePattern f = (FP.encodeString f) =~ ("\\.html$" :: String)
            slowly x = liftIO (threadDelay 10000) >> return x

type Airline = ByteString

data Record = Header
            | Carrier { carAirline :: Airline }
            | Leg
            | Segment
            | Trailer
            | Empty deriving Show

recordParser = choice [header, carrier, leg, segment, trailer, empty]
  where header  = P.char '1' >> P.take 199 >> P.endOfLine >> return Header
        carrier = do
          P.char '2'
          P.satisfy (\c -> c == 'U' || c == 'L')
          ad <- P.take 3
          P.take 195
          P.endOfLine
          return (Carrier ad)
        leg     = P.char '3' >> P.take 199 >> P.endOfLine >> return Leg
        segment = P.char '4' >> P.take 199 >> P.endOfLine >> return Segment
        trailer = P.char '5' >> P.take 199 >> P.endOfLine >> return Trailer
        empty   = P.endOfLine >> return Empty

conduitRecord = conduitParser recordParser =$= CL.map snd

resultConduit :: FP.FilePath -> IO [Record]
resultConduit s = runResourceT $ sourceFile s $= conduitRecord $$ CL.take 10

resultLazy :: FP.FilePath -> IO [Record]
resultLazy s = do
  c <- LB.readFile $ FP.encodeString s
  let r = LP.parse (many1 recordParser) c
  return . take 10 . fromMaybe [] $ LP.maybeResult r

main = defaultMain [
  bench "conduit" $ whnfIO $ length <$> resultConduit input,
  bench "lazy" $ whnfIO $ length <$> resultLazy input
  ]
  where input = "oag.ssim7.head"
