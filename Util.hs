module Util
where

import Data.Char(isSpace)
import Text.ParserCombinators.Parsec


notSpace = satisfy (not . isSpace)

spaces1 = many1 space

word = many1 notSpace

trim xs = reverse (trim' (reverse (trim' xs)))
  where
    trim' (x:xs) 
      | isSpace x = trim' xs
      | otherwise = x:xs

trimmed p = do
  chars <- p
  return $ trim chars

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
