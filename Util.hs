module Util
where

import Data.Char(isSpace)
import Text.ParserCombinators.Parsec


notSpace = satisfy (not . isSpace)

spaces1 = many1 space

word = many1 notSpace

trimmed = between spaces spaces

justParse parser x =
  case parse parser "" x of
    Right value -> value
    Left err -> error $ show err

maybeParser parser x =
  case parse parser "" x of
    Right value -> Just value
    Left _ -> Nothing
