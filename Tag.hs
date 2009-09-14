module Tag
where

import qualified SpecialChars as SC
import Text.ParserCombinators.Parsec
import Data.List(intercalate)
import Data.Set(Set, fromList, elems)

type Tag = String
type TagSet = Set Tag

tagSet = do
  inner <- SC.delimited 
  return $ fromList $ words inner

showTagSet :: TagSet -> String
showTagSet = SC.delimit . intercalate " " . elems 
