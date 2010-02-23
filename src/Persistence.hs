module Persistence
where

import qualified View as V
import qualified Database as D

import OS(dbPath)
import Util
import System.IO
import Text.Printf

open :: IO V.View
open = do 
  contents <- readFile dbPath 
  case simpleParse D.databaseP contents of
    Right db -> return $ V.fromDatabase db
    Left err -> fail $ "Parsing error when attempting to open '" ++ dbPath ++ "': " ++ err ++ "."

save :: V.View -> IO ()
save = writeFile dbPath . D.ugly . V.full
