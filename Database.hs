module Database
where

import qualified Tag as T
import qualified SpecialChars as SC

import Prelude hiding (filter)
import Maybe
import Util

import Text.ParserCombinators.Parsec
import Data.List(intercalate, sort, nub, intersect, (\\))
import qualified Data.Map as M
import qualified Data.Set as S

newtype Database = Database (M.Map T.TagSet String)

insertNewline a b = b ++ "\n" ++ a

database = do
  nullSection <- nullSection
  fullSections <- many fullSection
  let sections = fullSections ++ maybeToList nullSection 
  return $ Database (M.fromListWith insertNewline sections)
  where
    fullSection = do 
      header <- T.tagSet
      b <- body
      return $ (header, b)
    nullSection = do 
      spaces
      b <- body
      return $ if null b then Nothing else Just (S.empty, b)
    body = trimmed (many SC.escapedChar)

showDatabase (Database m) =
  intercalate "\n\n" $ map section (M.assocs m)
  where section (k, v) = T.showTagSet k ++ "\n" ++ v

-- Any string can be parsed as a database, so this is safe.
parseDatabase =
  justParse database

filter :: T.TagSet -> Database -> Database
filter tags (Database m) =
  Database (M.filterWithKey matchesTags m)
  where
    matchesTags key _ = tags `S.isSubsetOf` key 

prune :: T.TagSet -> Database -> Database
prune tags d =
  Database (M.mapKeys removeTags filtered)
  where 
    (Database filtered) = filter tags d
    removeTags key = S.difference key tags 

merge :: Database -> Database -> Database
merge (Database m1) (Database m2) =
  Database (merge' m1 m2)
  where merge' = M.unionWith insertNewline	

remove (Database garbage) (Database original) =
  Database (M.difference original garbage)

allTags :: Database -> S.Set T.Tag
allTags (Database m) = S.unions (M.keys m)

size :: Database -> Int
size (Database m) = M.size m
