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

import Data.ByteString (ByteString)

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
            filePattern f = (FP.encodeString f) =~ "\\.html$"
            slowly x = liftIO (threadDelay 10000) >> return x

go = runResourceT $ readAll $$ count

main = do
  result <- go
  putStrLn $ "Done: " ++ show result
