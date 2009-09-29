module SpecialChars
where

import Data.Char
import Text.ParserCombinators.Parsec

{- @ is a special character which denotes the start and end of a new tags section
   \@ is interpreted as a literal @
   for all other x, \x is interpreted as \x
   @ is the only character with special meaning
   @ is the only character that is escaped
-}

tagDelimiter = '@'

delimit s = tagDelimiter : s ++ [tagDelimiter]

escape = concatMap replaceAt
  where
    replaceAt x
      | x == tagDelimiter = '\\':[x]
      | otherwise         =      [x]

escapedChar =
  try (char '\\' >> char tagDelimiter)
  <|>
  satisfy (/= tagDelimiter)   

escapedCharNoSpace =
  try (char '\\' >> char tagDelimiter)
  <|>
  satisfy (\x -> x /= tagDelimiter && (not (isSpace x)))

delimited = brackets $ many escapedChar 
  where 
    bracket = char tagDelimiter 
    brackets = between bracket bracket
