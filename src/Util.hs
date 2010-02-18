module Util
where

import Data.Char(isSpace)
import Text.Parsec

trim = reverse . dropSpaces . reverse . dropSpaces
  where dropSpaces = dropWhile isSpace

notSpace :: Stream s m Char => ParsecT s u m Char
notSpace = satisfy (not . isSpace)

spaces1 :: Stream s m Char => ParsecT s u m String
spaces1 = many1 space

word :: Stream s m Char => ParsecT s u m String
word = many1 notSpace

simpleParse parser x =
  case parse parser "" x of
    Right value -> Right value
    Left err -> Left $ show err

justParse parser x =
  case parse parser "" x of
    Right value -> value
    Left err -> error $ show err

maybeParse parser x =
  case parse parser "" x of
    Right value -> Just value
    Left _ -> Nothing
