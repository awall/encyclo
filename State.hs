module State(
  State,
  parseState,
  showCurrent,
  showFull,
  showTags,
  saveState,
  openState,
  removeTags,
  possibleTags,
  addTags)
where

import qualified Database as D
import qualified Tag as T
import System.IO
import Data.List(intercalate)

data State = State { 
  persistent :: D.Database,
  current :: D.Database,
  tags :: T.TagSet }

parseState s = 
  State { persistent = db, current = db, tags = T.null }
  where db = D.parseDatabase s

showTags s = intercalate "/" $ T.unlock (tags s)

showFull s = D.showDatabase (persistent s)
showCurrent s = D.showDatabase (current s)

possibleTags s = D.possibleTags $ current s

removeTags ts s =
  s { tags = newTags,
      current = D.prune (T.unlock newTags) (persistent s) }
  where newTags = T.removeTags ts (tags s)

addTags ts s =
  s { tags = T.addTags ts (tags s),
      current = D.prune ts (current s) }

--
-- Persistence
--
saveState :: FilePath -> State -> IO ()
saveState path = writeFile path . D.showDatabase . persistent

openState :: FilePath -> IO State
openState path = do
  input <- readFile path
  return $ parseState input
