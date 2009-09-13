module Database
where

import Prelude as P
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

database = do
  nullSection <- nullSection
  fullSections <- many fullSection
  let sections = fullSections ++ maybeToList nullSection 
  return $ Database (M.fromListWith insertNewline sections)
  where
    insertNewline a b = b ++ "\n" ++ a
    fullSection = do 
      header <- tagSet
      body <- many escapedChar
      return $ (header, trim body)
    nullSection = do 
      spaces
      body <- many escapedChar
      let b = trim body
      return $ if P.null b then Nothing else Just (T.null, b)

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

prune tags d =
  Database (M.mapKeys (removeTags tags) filtered)
  where 
    (Database filtered) = filterDatabase tags d

possibleTags (Database m) =
  nub $ concat $ map unlock (M.keys m)
