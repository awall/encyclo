module Main
where

import qualified Persistence as P
import qualified View as V
import qualified Database as D
import qualified Command as C

import Util(justParse)
import System.IO
import Data.IORef
import Data.List(intercalate)
import Text.Printf

main = do
  P.open (\view -> do ref <- newIORef view 
                      let loop = do
                          input <- gatherInput ref
                          let f = justParse C.command input
                          catch (f ref loop) (\e -> print e >> loop)
                      loop)

-- User feedback
gatherInput ref = do
  state <- readIORef ref
  let tags = V.tags state
      dir  = intercalate "/" tags ++ "/"
      num  = D.size (V.current state)
  printf "%s [%d entries] >" dir num
  hFlush stdout
  getLine 
