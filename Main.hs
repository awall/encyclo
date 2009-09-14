module Main
where

import qualified State as S
import qualified Database as D
import qualified Tag as T

import System.Process
import System.IO
import System.Directory
import Data.IORef
import Data.List(intercalate, sort)
import Data.Set(elems)

main = do
  state <- open
  stateRef <- newIORef state 
  let loop = do
        input <- gatherInput stateRef
        let command = parseCommand input
        case command of
          Just f -> f stateRef loop
          Nothing -> putStrLn "invalid command" >> loop
  loop

-- IO
flush = hFlush stdout
newline = putStrLn ""
 
-- User feedback
gatherInput stateRef = do
  state <- readIORef stateRef
  let tags = S.tags state
      dir  = intercalate "/" tags ++ "/"
      num  = D.size (S.current state)
  putStr $ dir ++ " [" ++ show num ++ " entries] >"
  flush
  getLine 

-- Parsing Commands
parseCommand :: String -> Maybe (IORef S.State -> IO () -> IO ())
parseCommand ('c':'d':'+':' ':rest) = wrap $ cdPlus (words rest)
parseCommand ('c':'d':'-':' ':rest) = wrap $ cdMinus (words rest)
parseCommand input =
  case input of
    "quit" -> Just $ (\_ _ -> return ())
    "edit" -> wrap editInVim
    "save" -> wrapRO save
    "pwd"  -> wrapRO outCurrent
    "pfd"  -> wrapRO outFull
    "ls"   -> wrapRO outPossibleTags
    _      -> Nothing

-- Commands
wrap :: (IORef S.State -> IO ()) -> Maybe (IORef S.State -> IO () -> IO ())
wrap f = Just (\ref continue -> f ref >> continue)

wrapRO :: (S.State -> IO ()) -> Maybe (IORef S.State -> IO () -> IO ())
wrapRO f = Just (\ref continue -> readIORef ref >>= f >> continue)

outCurrent state = putStr (D.showDatabase $ S.current state) >> newline
outFull state = putStr (D.showDatabase $ S.full state) >> newline
outPossibleTags = putStrLn . intercalate "\n" . sort . elems . S.possibleTags

cdPlus  ts stateRef = modifyIORef stateRef (S.addTags ts)
cdMinus ts stateRef = modifyIORef stateRef (S.removeTags ts)

editInVim stateRef = do
  let editTemp path handle = do
        state <- readIORef stateRef
        let contents = D.showDatabase (S.currentWithFullPaths state)
        hPutStr handle contents
        hClose handle
        modifyIORef stateRef S.removeCurrent
        runVim path
        newContents <- readFile path
        let statePart = D.parseDatabase newContents
        modifyIORef stateRef (S.insert statePart)        
  withTemp editTemp

-- Read/Write state
dbPath = ".encyclo"
tempTemplate = "tmp.encyclo"

open :: IO S.State
open = do
  input <- readFile dbPath
  return $ S.fromDatabase $ D.parseDatabase input

save :: S.State -> IO ()
save state = 
  writeFile dbPath (D.showDatabase (S.full state))   

withTemp f = do
  (path, handle) <- openTempFile "." tempTemplate
  result <- f path handle
  removeFile path
  return result

runVim path = do 
  process <- runCommand $ "vim " ++ path
  waitForProcess process
  
