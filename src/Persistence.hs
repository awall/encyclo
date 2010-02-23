module Persistence
where

import qualified View as V
import qualified Database as D

import OS(dbPath)
import Util
import System.IO
import Text.Printf

open :: (V.View -> IO ()) -> IO ()
open withView = do 
  contents <- readFile dbPath 
  case simpleParse D.databaseP contents of
    Right db -> withView $ V.fromDatabase db
    Left err -> printf "Parsing error when attempting to open '%s': %s." dbPath err 

save :: V.View -> IO ()
save = writeFile dbPath . D.ugly . V.full
