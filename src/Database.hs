module Database(
  Database
  , Tag
  , Tags
  , empty
  , pretty
  , ugly
  , select
  , prune
  , merge
  , remove
  , size
  , tags
  , databaseP
) where

import qualified SpecialChars as SC

import Maybe
import Util

import Text.ParserCombinators.Parsec
import Data.List(intercalate, sort, nub, intersect, (\\))
import qualified Data.Map as M
import qualified Data.Set as S

type Tag = String
type Tags = S.Set Tag
type Database = M.Map Tags String

empty :: Database 
empty = M.empty 

ugly :: Database -> String
ugly db  = _showDatabase db id

pretty :: Database -> String
pretty db = _showDatabase db $ \s -> "\n" ++ trim s ++ "\n\n"

select :: Tags -> Database -> Database
select tags = M.filterWithKey $ \key _ -> tags `S.isSubsetOf` key

prune :: Tags -> Database -> Database
prune tags d = M.mapKeys (\key -> S.difference key tags) (select tags d)

merge :: Database -> Database -> Database
merge m1 m2 = (M.unionWith _combine) m1 m2

remove :: Database -> Database -> Database
remove = M.difference 

size :: Database -> Int
size = M.size

tags :: Database -> Tags
tags = S.unions . M.keys

-- This parser will not match on PARTs of strings, but only on the whole string itself.
-- In other words, the 'eof' marker in this parser means that this parser can not be
-- 'combined' inside other parsers, so 'many database', and other such nonsense, will 
-- simply not work. If it's necessary to do this, break this function up. 
databaseP :: Parser Database
databaseP = do
  spaces
  sections <- many section
  db <- return $ M.fromListWith _combine sections
  eof
  return db
  where section = do header <- (SC.delimitedP >>= return . S.fromList . words)
                     b <- many SC.escapedCharP
                     return (header, b)

_showDatabase m transform = concatMap section (M.assocs m)
  where section (k, v) = showTags k ++ transform v
        showTags = SC.delimit . intercalate " " . S.elems

_combine a b = b ++ a
