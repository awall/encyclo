module Command(
  command
, execute
)
where

import qualified Persistence as P
import qualified View as V
import qualified Database as D
import qualified Config as C

import OS(tempDbPath)
import Util
import Text.ParserCombinators.Parsec
import System.Process
import System.IO
import Data.IORef
import Data.Char
import Data.List
import Data.Set(elems)
import Text.Printf

data Command = Edit | Save | Cat | Ls | CdMinus [String] | CdPlus [String] | Cd [String]

command :: Parser Command
command =
  simple "edit" Edit     <|>
  simple "cat"  Cat      <|> 
  simple "ls"   Ls       <|> 
  simple "save" Save     <|> 

  withArgs "cd-" CdMinus <|>
  withArgs "cd+" CdPlus  <|>
  withArgs "cd"  Cd      

  where simple word command = try (string word >> return command)
        withArgs word command = try (string word >> args >>= return . command)
        args = (spaces1 >> word `sepBy` spaces1) <|> return []

--
-- Command's execution
--
execute :: Command -> IORef V.View -> IO ()

execute Cat ref = do
  view <- readIORef ref
  putStrLn $ D.pretty (V.current view)

execute Ls ref = do
  view <- readIORef ref
  putStrLn $ (intercalate "\n" . sort . elems) (V.possibleTags view)

execute Save ref = do
  view <- readIORef ref
  P.save view

execute (Cd tags) ref = modifyIORef ref (V.replaceTags tags)
execute (CdPlus tags) ref = modifyIORef ref (V.addTags tags)
execute (CdMinus tags) ref = modifyIORef ref (V.removeTags tags)

execute Edit ref = do
  view <- readIORef ref
  let contents = D.ugly (V.currentWithFullPaths view)
  writeFile tempDbPath contents
  runEditor tempDbPath
  newContents <- readFile tempDbPath
  case simpleParse D.databaseP newContents of
    Right db -> modifyIORef ref $ V.replaceCurrent db
    Left err -> printf "Failed to parse '%s': %s. Your changes were not persisted." tempDbPath err
  where runEditor path = 
          C.editCommand path >>= runCommand >>= waitForProcess
