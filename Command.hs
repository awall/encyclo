module Command(
  command
)
where

import qualified Persistence as P
import qualified State as S
import qualified Database as D
import qualified Tag as T

import Text.ParserCombinators.Parsec
import System.Process
import System.IO
import Data.IORef
import Data.Char
import Data.List
import Data.Set(elems)

type Ref = IORef S.State

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
notSpace = satisfy (not . isSpace)
spaces1 = many1 space
word = many1 notSpace
trimmed = between spaces spaces
args =
  do spaces1
     word `sepBy` spaces1
  <|> return []

--
-- Wrappers
--
wrapRO :: Parser (S.State -> IO ()) -> Parser (Ref -> IO () -> IO ())
wrapRO parser = do
  f <- trimmed parser
  return $ \ref continue -> do
    state <- readIORef ref
    f state
    continue

wrap :: Parser (Ref -> IO ()) -> Parser (Ref -> IO () -> IO ()) 
wrap parser = do 
  f <- trimmed parser
  return $ \ref continue -> do
    f ref
    continue

--
-- Command Parsers
--
quit = do
  trimmed $ string "quit"
  return $ const $ const $ return ()

invalid = do
  return $ const $ putStrLn "invalid command" 

cat = do
  string "cat"
  return $ putStrLn . D.showDatabase . S.current

ls = do
  string "ls"
  return $ putStrLn . intercalate "\n" . sort . elems . S.possibleTags 

cdish cmd f = do
  string cmd
  tags <- args
  return $ \ref -> modifyIORef ref (f tags)

cd      = cdish "cd"  S.replaceTags 
cdMinus = cdish "cd-" S.removeTags 
cdPlus  = cdish "cd+" S.addTags

edit = do
  string "edit"
  return $ \ref -> do
    state <- readIORef ref
    let contents = D.showDatabase (S.currentWithFullPaths state)
    writeFile P.tempPath contents
    modifyIORef ref S.removeCurrent
    runVim P.tempPath
    newContents <- readFile P.tempPath
    let statePart = D.parseDatabase newContents
    modifyIORef ref (S.insert statePart)   
  where
    runVim p = runCommand ("vim " ++ p) >>= waitForProcess

save = do
  string "save"
  return P.save