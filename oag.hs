import qualified Data.ByteString.Lazy.Char8 as C
import Data.Binary.Get

data LegElem = LegElem {
  legAirline :: String
} deriving Show

decodeLeg :: Get LegElem
decodeLeg = do
  a <- getLazyByteString 2
  return $ LegElem (C.unpack a)

type Block = C.ByteString
type Element = C.ByteString

decodeBlocks :: Get [Block]
decodeBlocks = do
  x <- getLazyByteString 1000
  xs <- decodeBlocks
  return (x:xs)

main :: IO ()
main = do
  bin <- C.getContents
  let blocks = takeWhile (not . C.null) $ runGet decodeBlocks bin
  print blocks
