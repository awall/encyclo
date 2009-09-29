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
  deriving Eq

instance Show Database where
  show = showDatabase

nilDatabase = Database (M.empty)

insertNewline a b = b ++ "\n" ++ a

-- This parser will not match on PARTs of strings, but only on the whole string itself.
-- In other words, the 'eof' marker in this parser means that it this parser can not be
-- 'combined' inside other parsers, so 'many database', and other such nonsense, will 
-- simply not work. If it's necessary to do this, exposing database' is the answer.
database = do
  db <- database'
  eof
  return db

database' = do
  spaces
  sections <- many section
  return $ Database (M.fromListWith insertNewline sections)
  where
    section = do header <- T.tagSet
                 b <- trimmed $ many SC.escapedChar
                 return (header, b)

showDatabase (Database m) =
  intercalate "\n\n" $ map section (M.assocs m)
  where section (k, v) = T.showTagSet k ++ "\n" ++ v

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
