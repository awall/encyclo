module Database
where

import Util(join, trim)
import SpecialChars(escapedChar)
import Tag(TagSet, tagSet, containsTag, showTagSet)
import Text.ParserCombinators.Parsec
import qualified Data.Map as M

type Body = String

newtype Database = Database (M.Map TagSet Body)
  deriving Eq

database = do
  sections <- many section
  return $ Database (M.fromListWith insertNewline sections)
  where
    insertNewline a b = b ++ "\n" ++ a
    section = do
      header <- tagSet
      body <- many escapedChar
      return $ (header, trim body)

showDatabase (Database m) =
  join "\n" $ map section (M.assocs m)
  where section (k, v) = showTagSet k ++ "\n" ++ v

instance Show Database where
  show = showDatabase

filterDatabase tags (Database m) =
  Database (M.filterWithKey matchesTags m)
  where
    matchesTags key _ =
      any (\t -> containsTag t key) tags
