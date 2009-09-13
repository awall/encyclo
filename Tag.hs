module Tag(
  TagSet,
  tagSet,
  showTagSet,
  containsTag)
where

import Util(join)
import SpecialChars(delimited, delimit)
import Text.ParserCombinators.Parsec
import Data.List(sort, nub)

type Tag = String
newtype TagSet = TagSet String
  deriving (Eq, Ord)

tagSet = delimited >>= return . lock . words

lock :: [Tag] -> TagSet
lock = TagSet . join " " . sort . nub

unlock :: TagSet -> [Tag]
unlock (TagSet s) = words s

containsTag :: Tag -> TagSet -> Bool
containsTag tag key = tag `elem` unlock key

showTagSet :: TagSet -> String
showTagSet (TagSet s) = delimit s
