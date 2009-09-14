module Util
where

import Data.Char(isSpace)
import Text.ParserCombinators.Parsec

trim = dropSpace . reverse . dropSpace . reverse
  where dropSpace = dropWhile isSpace

justParse parser x =
  case parse parser "" x of
    Right value -> value
    Left err -> error $ show err
