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
  , add
  , disjoint
  , singleton
  , databaseP
) where

import qualified SpecialChars as SC

import Maybe
import Util

import Test.QuickCheck
import Test.HUnit
import Text.ParserCombinators.Parsec
import Data.List(null, delete, intercalate, sort, nub, intersect, (\\))
import qualified Data.Map as M
import qualified Data.Set as S

type Tag = String
type Tags = S.Set Tag
newtype Database = Database (M.Map Tags String)
  deriving Eq

instance Show Database where
  show = ugly

empty :: Database 
empty = Database M.empty 

ugly :: Database -> String
ugly db  = _showDatabase db id

pretty :: Database -> String
pretty db = _showDatabase db $ \s -> "\n" ++ trim s ++ "\n\n"

select :: Tags -> Database -> Database
select tags (Database m) = 
  Database $ M.filterWithKey (\key _ -> tags `S.isSubsetOf` key) m

prune :: Tags -> Database -> Database
prune tags db = 
  Database $ M.mapKeys (\key -> S.difference key tags) selected
  where Database selected = select tags db

merge :: Database -> Database -> Database
merge (Database m1) (Database m2) = Database $ (M.unionWith _combine) m2 m1

remove :: Database -> Database -> Database
remove (Database m1) (Database m2) = Database $ M.difference m1 m2

add :: Tags -> String -> Database -> Database
add tags content (Database m) = Database $ (M.unionWith _combine) m (M.singleton tags content)

singleton :: Tags -> String -> Database
singleton tags content = add tags content empty

size :: Database -> Int
size (Database m) = M.size m

tags :: Database -> Tags
tags (Database m) = S.unions $ M.keys m

disjoint :: Database -> Database -> Bool
disjoint (Database m1) (Database m2) = null $ intersect (M.keys m1) (M.keys m2) 

-- This parser will not match on PARTs of strings, but only on the whole string itself.
-- In other words, the 'eof' marker in this parser means that this parser can not be
-- 'combined' inside other parsers, so 'many database', and other such nonsense, will 
-- simply not work. If it's necessary to do this, break this function up. 
databaseP :: Parser Database
databaseP = do
  spaces
  sections <- many section
  let m = M.fromListWith _combine sections
  eof
  return $ Database m
  where section = do header <- (SC.delimitedP >>= return . _textToTags)
                     b <- many SC.escapedP
                     return (header, b)

----------------------------------------------------------------------------------------------------
-- Utility Functions
----------------------------------------------------------------------------------------------------
_showDatabase (Database m) transform = concatMap section (M.assocs m)
  where section (k, v) = showTags k ++ (SC.escape $ transform v)
        showTags = SC.delimit . intercalate " " . S.elems

_textToTags = S.fromList . nub . words

_combine a b = b ++ a

pd = justParse databaseP
md = maybeParse databaseP

----------------------------------------------------------------------------------------------------
-- QuickCheck
----------------------------------------------------------------------------------------------------
instance Arbitrary Database where
  arbitrary = gen_weirdDatabase

-- For all databases, parsing what we output produces the original database
prop_readWrite db = db == pd (ugly db)

-- For all databases, merge empty db == db
prop_mergeEmpty db = merge empty db == db

-- For all disjoint databases, merging is reflexive. Reflexivity is not generally true, because
-- merging two databases that have the same keys merges the contents of those keys in an 
-- order-dependent fashion (the contents are combined using '++').
prop_mergeReflexive (d1, d2) =
  disjoint d1 d2 ==> merge d1 d2 == merge d2 d1

-- For all databases, merge (singleton t c) db == add t c db
prop_mergeAdd = forAll (tupleC gen_weirdDatabase gen_weirdString) $ \(db, str) ->
  let tags = _textToTags str
      c = "content" in
  merge (singleton tags c) db == add tags c db

-- For all pairs of strings (a,b), where a and b represent 'equivalent' headers, ensure
-- that the produced databases actually are equivalent. The 'equivalent' headers will
-- feature the same arguments, but they may sometimes be in a different order or repeated a 
-- different number of times.
prop_shuffle = 
  forAll gen_multiDBTextPair $ \(t1, t2) -> parseEq t1 t2
  where parseEq a b = pd a == pd b

-- For all pairs of strings (a,b), where a and b represent 'equivalent' headers, ensure
-- that the produced databases actually are not only equivalent, but also that they
-- are printed out identically.
prop_shuffleUgly = 
  forAll gen_multiDBTextPair $ \(t1, t2) -> displayEq t1 t2
  where displayEq a b = ugly (pd a) == ugly (pd b)

prop_shufflePretty = 
  forAll gen_multiDBTextPair $ \(t1, t2) -> displayEq t1 t2
  where displayEq a b = pretty (pd a) == pretty (pd b)

-- Ensure all equivalent databases are printed out identically.
times n m = sequence $ replicate n m

tagsC gen = do
  s <- gen
  return (_textToTags s)

tupleC genA genB = do
  a <- genA
  b <- genB
  return (a, b)

shuffleC genL = do
  list <- genL
  shuffleC' list
  where shuffleC' [] = return []
        shuffleC' l  = do index <- choose (0, length(l) - 1)
                          let item = l !! index
                          rest <- shuffleC' (delete item l)
                          return $ item : rest

toDBText a = SC.delimit a ++ "content"

gen_weirdString = 
  oneof $ map (`times` char) [0, 1, 2, 3, 4, 30, 50, 100] 
  where char = elements $ SC.header : SC.pre : "\\\0\t\n\r ./aAbB@cC"

-- Generate a database using weird characters that could cause parsing/writing problems
gen_weirdDatabase = do
  s <- gen_sections 
  return $ Database (M.fromListWith _combine s)
  where gen_tags = tagsC gen_weirdString
        gen_both = tupleC gen_tags gen_weirdString 
        gen_sections = oneof $ map (`times` gen_both) [0, 1, 2, 3]

-- Generate a "canned" header string, every tag set produced with this generator and the same int argument
-- should contain the same words, but they may be in a different order and repeated a different number of times
gen_cannedTags :: Int -> Gen String
gen_cannedTags n = do
  llw <- mapM word [1..n]
  let header = concat llw
  shuffledHeader <- shuffleC (return header) 
  return $ intercalate " " shuffledHeader
  where word x = oneof $ map (`times` (return $ "w" ++ show x)) [1,2,3,4]  

gen_cannedDBTextPair = do
  n <- choose (0, 6)
  a <- gen_cannedTags n
  b <- gen_cannedTags n
  return (toDBText a, toDBText b)

gen_multiDBTextPair = do
  n <- choose (0, 6)
  cs <- n `times` gen_cannedDBTextPair
  let (c1s, c2s) = unzip cs
  s1 <- shuffleC $ return c1s
  s2 <- shuffleC $ return c2s
  return (concat s1, concat s2)

----------------------------------------------------------------------------------------------------
-- Manual Unit Tests
----------------------------------------------------------------------------------------------------
test_blank =
  version1 ~=? version2
  where
    version1 = empty
    version2 = pd ""

test_combine =
  version1 ~=? version2
  where
    version1 = pd "|tag1|blah1|tag1|blah2|tag1|blah3"
    version2 = pd "|tag1|blah1blah2blah3"

test_preSpaces =
  version1 ~=? version2
  where
    version1 = pd "     |tag1|body1"
    version2 = pd "|tag1|body1"

test_preTag =
  Nothing ~=? version1
  where
    version1 = md "blahblah|tag1|body1"

test_unclosed =
  version1 ~=? Nothing
  where
    version1 = md "|unclosed tags here"

test_merged =
  piece12 ~=? merge piece1 piece2
  where
    piece1 = pd "|tag1|123|tag2|456|tag1|789"
    piece2 = pd "|tag2|def|tag1|abc"
    piece12 = pd "|tag1|123789abc|tag2|456def"
