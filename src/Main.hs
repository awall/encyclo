module Main
where

import qualified Persistence as P
import qualified View as V
import qualified Database as D
import qualified Command as C

import Control.Monad(unless)
import Util(maybeParse)
import System.IO
import Data.IORef
import Data.List(intercalate)
import Text.Printf

main = do
  view <- P.open
  ref <- newIORef view 
  let loop = do
      input <- gatherInput ref
      unless (input == "quit") $ do
        case maybeParse C.command input of
             Just c -> catch (C.execute c ref) print
             Nothing -> putStrLn "Invalid input."
        loop 
  loop

-- User feedback
gatherInput ref = do
  state <- readIORef ref
  let tags = V.tags state
      dir  = intercalate "/" tags ++ "/"
      num  = D.size (V.current state)
  printf "%s [%d entries] >" dir num
  hFlush stdout
  getLine 
