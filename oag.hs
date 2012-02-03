import Prelude hiding (take) -- conflicting with Attoparsec
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Lazy.Char8 as LB (ByteString, getContents)
import Data.Attoparsec.ByteString.Char8 (take, Parser)
import Data.Attoparsec.ByteString.Lazy (maybeResult, parse, Result (Done))

data LegElem = LegElem {
  legAirline :: !ByteString
} deriving Show

parseElem :: Parser LegElem
parseElem = do
  a <- take 2
  return $ LegElem a

go :: LB.ByteString -> Maybe LegElem
go = maybeResult . (parse parseElem)

main :: IO ()
main = do
  c <- LB.getContents
  print $ go c
