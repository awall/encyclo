module View
where

import qualified Database as D

import Data.List(nub, (\\))
import Data.Set(fromList)

data View = View D.Database [D.Tag]

fromDatabase db = View db []

full (View db _) = db
current (View db ts) = D.prune (fromList ts) db
currentWithFullPaths (View db ts) = D.select (fromList ts) db

tags (View _ ts) = ts
possibleTags (View db ts) = D.tags (D.prune (fromList ts) db)

removeCurrent s = mapDb (`D.minus` currentWithFullPaths s) s

insert newDb = mapDb (D.merge newDb)
replaceCurrent new s = insert new (removeCurrent s)

removeTags garbage = mapTags (\\ garbage)
addTags new = mapTags (++ new)
replaceTags new = mapTags (const new)

mapTags f (View db ts) = View db (nub $ f ts)
mapDb f (View db ts) = View (f db) ts
