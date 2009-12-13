module SpecialChars
where

import Data.Char
import Text.ParserCombinators.Parsec

--
-- | is a special character which denotes the start and end of a new tags section.
-- For all x, `x is interpreted as x.
-- To include | in your writing, users must write `|
-- To include ` in your writing, users must write ``
--
-- There is NO OTHER special syntax.
--
pre = '`'
header = '|'

delimit :: String -> String
delimit s = header : escape s ++ [header]

escape :: String -> String 
escape = concatMap replaceAt
  where
    replaceAt x
      | x == header = [pre,header] 
      | x == pre    = [pre,pre]
      | otherwise   = [x]

escapedP :: Parser Char
escapedP =
  try (char pre >> anyChar)
  <|>
  satisfy (/= header)   

delimitedP :: Parser String
delimitedP = brackets $ many escapedP
  where 
    bracket = char header 
    brackets = between bracket bracket
