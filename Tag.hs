module Tag
where

import SpecialChars(delimited, delimit)
import Text.ParserCombinators.Parsec
import Data.List(intercalate)
import qualified Data.Set as S

type Tag = String
type TagSet = S.Set Tag

tagSet = do
  inner <- delimited 
  return $ S.fromList $ words inner

showTagSet :: TagSet -> String
showTagSet = delimit . intercalate " " . S.elems 
