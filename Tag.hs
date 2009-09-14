module Tag
where

import SpecialChars(delimited, delimit)
import Text.ParserCombinators.Parsec
import Data.List(intercalate, sort, nub)

type Tag = String

tags = do
  inner <- delimited 
  return $ sort (nub (words inner))

showTags :: [Tag] -> String
showTags = delimit . intercalate " " 
