module State
where

import qualified Database as D
import qualified Tag as T
import System.IO
import Data.List(intercalate, nub, (\\))

data State = State {
  db :: D.Database, 
  tags :: [T.Tag] 
}

parseState s = State (D.parseDatabase s) []

showTags = intercalate "/" . tags 

showFull = D.showDatabase . db
showCurrent (State db tags)  = D.showDatabase (D.prune tags db)
showCurrentWithFullPaths (State db tags) = D.showDatabase (D.filter tags db) 
possibleTags (State db tags) = D.possibleTags (D.prune tags db)

withTags f (State db tags) =
  State db (nub $ f tags)

withDB f (State db tags) = 
  State (f db) tags

removeCurrent (State db tags) =
  State (D.remove (D.filter tags db) db) tags

insert newDB = withDB (D.merge newDB)

removeTags garbage = withTags (\\ garbage)
addTags new = withTags (++ new)

--
-- Persistence
--
saveState :: FilePath -> State -> IO ()
saveState path = writeFile path . D.showDatabase . db

openState :: FilePath -> IO State
openState path = do
  input <- readFile path
  return $ parseState input
