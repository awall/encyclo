module Command(
  command
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
import Data.List
import Data.Set(elems)
import Text.Printf

command :: Parser (IORef V.View -> IO ())
command =
  simple "edit" edit     <|>
  simple "cat"  cat      <|> 
  simple "ls"   ls       <|> 
  simple "save" save     <|> 

  withArgs "cd-" cdMinus <|>
  withArgs "cd+" cdPlus  <|>
  withArgs "cd"  cd      

  where simple word command = try (string word >> return command)
        withArgs word command = try (string word >> args >>= return . command)
        args = (spaces1 >> word `sepBy` spaces1) <|> return []

--
-- Command's execution
--
cat ref = do
  view <- readIORef ref
  putStrLn $ D.pretty (V.current view)

ls ref = do
  view <- readIORef ref
  putStrLn $ (intercalate "\n" . sort . elems) (V.possibleTags view)

save ref = do
  view <- readIORef ref
  P.save view

cd tags ref = modifyIORef ref (V.replaceTags tags)
cdPlus tags ref = modifyIORef ref (V.addTags tags)
cdMinus tags ref = modifyIORef ref (V.removeTags tags)

edit ref = do
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
