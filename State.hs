module State
where

import qualified Database as D
import qualified Tag as T

import Data.List(nub, (\\))
import Data.Set(fromList)

data State = State D.Database [T.Tag]

fromDatabase db = State db []

tags (State _ ts) = ts
full (State db _) = db

current (State db ts) =
  D.prune (fromList ts) db

currentWithFullPaths (State db ts) =
  D.filter (fromList ts) db

possibleTags (State db ts) = 
  D.allTags (D.prune (fromList ts) db)

removeCurrent (State db ts) =
  State (D.remove (D.filter (fromList ts) db) db) ts

insert newDB (State db ts) = 
  State (D.merge newDB db) ts

removeTags garbage (State db ts) = 
  State db (ts \\ garbage)
  
addTags new (State db ts) = 
  State db (nub $ ts ++ new)

replaceTags new (State db _) =
  State db (nub $ new)
