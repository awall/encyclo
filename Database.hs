module Database
where

import Prelude hiding (filter)
import Maybe
import Util(trim, justParse)
import SpecialChars(escapedChar)
import qualified Tag as T
import Text.ParserCombinators.Parsec
import Data.List(intercalate, sort, nub, intersect, (\\))
import qualified Data.Map as M

type Body = String
type TagsKey = String

lock :: [T.Tag] -> TagsKey
lock = intercalate " " . sort . nub 

unlock :: TagsKey -> [T.Tag]
unlock = words 

newtype Database = Database (M.Map TagsKey Body)
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
      header <- T.tags
      body <- many escapedChar
      return $ (lock $ header, trim body)
    nullSection = do 
      spaces
      body <- many escapedChar
      let b = trim body
      return $ if null b then Nothing else Just ("", b)

showDatabase (Database m) =
  intercalate "\n\n" $ map section (M.assocs m)
  where section (k, v) = T.showTags (unlock k) ++ "\n" ++ v

--
-- Any string can be parsed as a database, so this is safe.
--
parseDatabase =
  justParse database

instance Show Database where
  show = showDatabase

filter tags (Database m) =
  Database (M.filterWithKey matchesTags m)
  where
    matchesTags key _ = not $ null $ intersect (unlock key) tags

prune tags d
  | null tags = d
  | otherwise =	
      Database (M.mapKeys removeTags filtered)
      where 
        (Database filtered) = filter tags d
        removeTags key = lock $ (\\ tags) $ unlock key

merge (Database m1) (Database m2) =
  Database (merge' m1 m2)
  where merge' = M.unionWith insertNewline	

remove (Database garbage) (Database original) =
  Database (M.difference original garbage)

possibleTags (Database m) =
  nub $ concat $ map unlock (M.keys m)
