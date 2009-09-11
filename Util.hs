module Util
where

import Data.Char(isSpace)
import Data.List(intersperse)
import Text.ParserCombinators.Parsec

join x ls = concat $ intersperse x ls

trim = dropSpace . reverse . dropSpace . reverse
  where dropSpace = dropWhile isSpace

justParse parser x =
  case parse parser "" x of
    Right value -> value
    Left err -> error $ show err
