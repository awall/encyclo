module SpecialChars
where

import Data.Char
import Text.ParserCombinators.Parsec

-- @ is a special character which denotes the start and end of a new tags section
-- \@ is interpreted as a literal @
-- for all other x, \x is interpreted as \x
-- @ is the only character with special meaning
-- @ is the only character that is escaped

tagDelimiter :: Char
tagDelimiter = '@'

delimit :: String -> String
delimit s = tagDelimiter : s ++ [tagDelimiter]

escape :: String -> String 
escape = concatMap replaceAt
  where
    replaceAt x
      | x == tagDelimiter = '\\':[x]
      | otherwise         =      [x]

escapedCharP :: Parser Char
escapedCharP =
  try (char '\\' >> char tagDelimiter)
  <|>
  satisfy (/= tagDelimiter)   

escapedCharNoSpaceP :: Parser Char
escapedCharNoSpaceP =
  try (char '\\' >> char tagDelimiter)
  <|>
  satisfy (\x -> x /= tagDelimiter && (not (isSpace x)))

delimitedP :: Parser String
delimitedP = brackets $ many escapedCharP
  where 
    bracket = char tagDelimiter 
    brackets = between bracket bracket
