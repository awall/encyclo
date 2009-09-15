module Persistence
where

import qualified State as S
import qualified Database as D
import System.IO

dbPath = ".encyclo"
tempPath = ".encyclotemp"

open :: IO S.State
open = readFile dbPath >>= return . S.fromDatabase . D.parseDatabase

save :: S.State -> IO ()
save = writeFile dbPath . D.showDatabase . S.full
