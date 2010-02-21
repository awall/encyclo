module Main
where

import qualified Persistence as P
import qualified State as S
import qualified Database as D
import qualified Command as C

import Util(justParse)
import System.IO
import Data.IORef
import Data.List(intercalate)
import Text.Printf

main = do
  P.open (\state -> do ref <- newIORef state 
                       let loop = do
                           input <- gatherInput ref
                           let f = justParse C.command input
                           catch (f ref loop) (\e -> print e >> loop)
                       loop)

-- User feedback
gatherInput ref = do
  state <- readIORef ref
  let tags = S.tags state
      dir  = intercalate "/" tags ++ "/"
      num  = D.size (S.current state)
  printf "%s [%d entries] >" dir num
  hFlush stdout
  getLine 
