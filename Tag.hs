module Tag
where

import SpecialChars(delimited, delimit)
import Text.ParserCombinators.Parsec
import Data.List(sort, nub, delete, intercalate, (\\))

type Tag = String
newtype TagSet = TagSet String
  deriving (Eq, Ord)

nilTagSet = TagSet ""
nullTagSet (TagSet "") = True
nullTagSet _           = False

tagSet = delimited >>= return . lock . words

showTagSet :: TagSet -> String
showTagSet (TagSet s) = delimit s

lock :: [Tag] -> TagSet
lock = TagSet . intercalate " " . sort . nub

unlock :: TagSet -> [Tag]
unlock (TagSet s) = words s

mergeTags :: TagSet -> TagSet -> TagSet
mergeTags t1 t2 =
  lock $ unlock t1 ++ unlock t2

containsTag :: Tag -> TagSet -> Bool
containsTag tag key = tag `elem` unlock key

removeTag :: Tag -> TagSet -> TagSet
removeTag t ts = lock $ delete t $ unlock ts

removeTags :: [Tag] -> TagSet -> TagSet
removeTags ts set = lock $ (\\ ts) $ unlock set

addTags :: [Tag] -> TagSet -> TagSet
addTags ts set = lock $ (++ ts) $ unlock set
