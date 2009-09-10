module Tag
where

import SpecialChars(tagsSection)
import Text.ParserCombinators.Parsec

tags = do
  inner <- tagsSection 
  return $ words inner
