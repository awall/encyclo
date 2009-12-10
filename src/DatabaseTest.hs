module DatabaseTest
where

import qualified Database as D

import Text.ParserCombinators.Parsec
import Test.HUnit
import Util

pd = justParse D.databaseP
md = maybeParse D.databaseP

tests = TestList [
  "readWrite" ~: readWrite,
  "multitag"  ~: multitag ,
  "combine"   ~: combine  ,
  "spaces"    ~: preSpaces,
  "pretag"    ~: preTag   ,
  "unclosed"  ~: unclosed ,
  "empty"     ~: empty    ,
  "order"     ~: order    ]

empty =
  version1 ~=? version2
  where
    version1 = D.empty
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
    version2 = pd "@tag1@blah1blah2blah3"

readWrite =
  version1 ~=? version2
  where
    version1 = pd "@tag1@\n\t  \nbl\t\nah\t\n  @tag2 tag3@blah"
    version2 = pd $ D.ugly version1 

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
