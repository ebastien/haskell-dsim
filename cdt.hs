import Data.Conduit
import Data.Conduit.Filesystem (traverse, sourceFile)
import Control.Monad.IO.Class (liftIO)
import System.Environment (getArgs)
import qualified Data.Conduit.Binary as CB
import qualified Data.Conduit.List as CL
import qualified Filesystem.Path.CurrentOS as FP

import Text.Regex.Posix ((=~))

import Control.Concurrent (threadDelay)

import Debug.Trace (trace)

go = runResourceT $ sourceArgs
      $= awaitForever sourceDir
      $= CL.filter filePattern
      $= CL.mapM slowly
      $= awaitForever sourceFile
      $= CB.lines
      $$ CL.fold (\n _ -> n + 1 ) 0
      where sourceArgs = liftIO getArgs >>= mapM_ yield
            sourceDir f = traverse False (FP.decodeString f)
            filePattern f = (FP.encode f) =~ "\\.html$"
            slowly x = liftIO (threadDelay 10000) >> return x

main = do
  result <- go
  putStrLn $ "Done: " ++ show result
