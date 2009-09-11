module Tag
where

import Util(join)
import SpecialChars(delimited, delimit)
import Text.ParserCombinators.Parsec
import Data.List(sort)

type Tag = String
type Tags = [Tag]
type TagsKey = String

tags = delimited >>= return . words
tagsKey = tags >>= return . lock

lock :: Tags -> TagsKey
lock = join " " . sort

unlock :: TagsKey -> Tags
unlock s = words s

containsTag :: Tag -> TagsKey -> Bool
containsTag tag key = tag `elem` unlock key

showTagsKey :: TagsKey -> String
showTagsKey = delimit
