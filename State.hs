module State(
  State,
  parseState,
  showCurrent,
  showFull,
  showTags,
  saveState,
  openState,
  removeTags,
  addTags)
where

import Database
import qualified Tag as T
import System.IO
import Data.List(intercalate)

data State = State { 
  persistent :: Database,
  current :: Database,
  tags :: T.TagSet }

parseState s = 
  State { persistent = db, current = db, tags = T.null }
  where db = parseDatabase s

showTags s = intercalate "/" $ T.unlock (tags s)

showFull s = showDatabase (persistent s)
showCurrent s = showDatabase (current s)

removeTags ts s =
  s { tags = newTags,
      current = prune (T.unlock newTags) (persistent s) }
  where newTags = T.removeTags ts (tags s)

addTags ts s =
  s { tags = T.addTags ts (tags s),
      current = prune ts (current s) }

--
-- Persistence
--
saveState :: FilePath -> State -> IO ()
saveState path = writeFile path . showDatabase . persistent

openState :: FilePath -> IO State
openState path = do
  input <- readFile path
  return $ parseState input
