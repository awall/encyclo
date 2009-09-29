module Persistence
where

import qualified State as S
import qualified Database as D

import Util
import System.IO
import Text.Printf

dbPath = ".encyclo"
tempPath = ".encyclotemp"

open :: (S.State -> IO ()) -> IO ()
open withState = do 
  contents <- readFile dbPath 
  case simpleParse D.database contents of
    Right db -> withState $ S.fromDatabase db
    Left err -> printf "Parsing error when attempting to open '%s': %s." dbPath err 

save :: S.State -> IO ()
save = writeFile dbPath . D.showDatabase . S.full
