module Database
where

import Maybe
import Util(trim, justParse)
import SpecialChars(escapedChar)
import Tag as T
import Text.ParserCombinators.Parsec
import Data.List(intercalate, nub)
import qualified Data.Map as M

type Body = String

newtype Database = Database (M.Map TagSet Body)
  deriving Eq

nilDatabase = Database (M.empty)

insertNewline a b = b ++ "\n" ++ a

database = do
  nullSection <- nullSection
  fullSections <- many fullSection
  let sections = fullSections ++ maybeToList nullSection 
  return $ Database (M.fromListWith insertNewline sections)
  where
    fullSection = do 
      header <- tagSet
      body <- many escapedChar
      return $ (header, trim body)
    nullSection = do 
      spaces
      body <- many escapedChar
      let b = trim body
      return $ if null b then Nothing else Just (T.nilTagSet, b)

showDatabase (Database m) =
  intercalate "\n\n" $ map section (M.assocs m)
  where section (k, v) = showTagSet k ++ "\n" ++ v

--
-- Any string can be parsed as a database, so this is safe.
--
parseDatabase =
  justParse database

instance Show Database where
  show = showDatabase

filterDatabase tags (Database m) =
  Database (M.filterWithKey matchesTags m)
  where
    matchesTags key _ =
      any (\t -> containsTag t key) tags

prune tags d
  | nullTagSet (lock $ tags) = d
  | otherwise =	
      Database (M.mapKeys (removeTags tags) filtered)
      where 
        (Database filtered) = filterDatabase tags d

merge (Database m1) (Database m2) =
  Database (merge' m1 m2)
  where merge' = M.unionWith insertNewline	

remove (Database garbage) (Database original) =
  Database (M.difference original garbage)

injectTags tags (Database m) =
  Database (M.mapKeysWith insertNewline (T.mergeTags tags) m)

possibleTags (Database m) =
  nub $ concat $ map unlock (M.keys m)
