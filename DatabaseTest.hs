module DatabaseTest
where

import Database
import Text.ParserCombinators.Parsec
import Test.HUnit
import Util

pd = justParse database
md = maybeParse database

tests = TestList [
  "readWrite" ~: readWrite,
  "multitag"  ~: multitag ,
  "newlines"  ~: newlines ,
  "combine"   ~: combine  ,
  "spaces"    ~: preSpaces,
  "pretag"    ~: preTag   ,
  "unclosed"  ~: unclosed ,
  "empty"     ~: empty    ,
  "order"     ~: order    ]

empty =
  version1 ~=? version2
  where
    version1 = nilDatabase
    version2 = pd ""

order =
  version1 ~=? version2
  where
    version1 = pd "@tag1@[body1]@tag2@[body2]" 
    version2 = pd "@tag2@[body2]@tag1@[body1]" 

combine =
  version1 ~=? version2
  where
    version1 = pd "@tag1@blah1@tag1@blah2@tag1@blah3"
    version2 = pd "@tag1@blah1\nblah2\nblah3"

newlines =
  version1 ~=? version2
  where
    version1 = pd "@tag1@\n\t \r \nbl\t\nah\t\r\n  @tag2@blah"
    version2 = pd "@tag1@bl\t\nah\n@tag2@\n\rblah\t\n"

readWrite =
  version1 ~=? version2
  where
    version1 = pd "@tag1@\n\t  \nbl\t\nah\t\n  @tag2 tag3@blah"
    version2 = pd $ showDatabase version1 

multitag =
  version1 ~=? version2
  where
    version1 = pd "@tag1 tag1@body1@tag2 tag2 tag2@body2"
    version2 = pd "@tag1@body1@tag2@body2"

preSpaces =
  version1 ~=? version2
  where
    version1 = pd "     @tag1@body1"
    version2 = pd "@tag1@body1"

preTag =
  Nothing ~=? version1
  where
    version1 = md "blahblah@tag1@body1"

unclosed =
  version1 ~=? Nothing
  where
    version1 = md "@unclosed tags here"
