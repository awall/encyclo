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
import Data.Char
import Data.List
import Data.Set(elems)
import Text.Printf

type Ref = IORef V.View

command :: Parser (Ref -> IO () -> IO ())
command =
  try (wrap edit) 
  <|> try (wrapRO cat)
  <|> try (wrapRO ls)
  <|> try (wrapRO save)
  <|> try (wrap cdMinus)
  <|> try (wrap cdPlus)
  <|> try (wrap cd)
  <|> try quit
  <|> wrap invalid

--
-- Utility parsers
--
args =
  do spaces1
     word `sepBy` spaces1
  <|> return []

--
-- Wrappers
--
wrapRO :: Parser (V.View -> IO ()) -> Parser (Ref -> IO () -> IO ())
wrapRO parser = do
  f <- parser
  return $ \ref continue -> do
    state <- readIORef ref
    f state
    continue

wrap :: Parser (Ref -> IO ()) -> Parser (Ref -> IO () -> IO ()) 
wrap parser = do 
  f <- parser
  return $ \ref continue -> do
    f ref
    continue

--
-- Command Parsers
--
quit = do
  string "quit"
  return $ const $ const $ return ()

invalid = do
  return $ const $ putStrLn "invalid command" 

cat = do
  string "cat"
  return $ putStrLn . D.pretty . V.current

ls = do
  string "ls"
  return $ putStrLn . intercalate "\n" . sort . elems . V.possibleTags 

cdish cmd f = do
  string cmd
  tags <- args
  return $ \ref -> modifyIORef ref (f tags)

cd      = cdish "cd"  V.replaceTags 
cdMinus = cdish "cd-" V.removeTags 
cdPlus  = cdish "cd+" V.addTags

edit = do
  string "edit"
  return $ \ref -> do
    state <- readIORef ref
    let contents = D.ugly (V.currentWithFullPaths state)
    writeFile tempDbPath contents
    runEditor tempDbPath
    newContents <- readFile tempDbPath
    case simpleParse D.databaseP newContents of
      Right db -> modifyIORef ref $ V.replaceCurrent db
      Left err -> printf "Failed to parse '%s': %s. Your changes were not persisted." tempDbPath err
  where runEditor path = 
          C.editCommand path >>= runCommand >>= waitForProcess

save = do
  string "save"
  return P.save
