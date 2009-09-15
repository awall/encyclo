module Main
where

import qualified State as S
import qualified Database as D

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
    "cat"  -> wrapRO outCurrent
    "ls"   -> wrapRO outPossibleTags
    _      -> Nothing

-- Commands
wrap :: (IORef S.State -> IO ()) -> Maybe (IORef S.State -> IO () -> IO ())
wrap f = Just (\ref continue -> f ref >> continue)

wrapRO :: (S.State -> IO ()) -> Maybe (IORef S.State -> IO () -> IO ())
wrapRO f = Just (\ref continue -> readIORef ref >>= f >> continue)

outCurrent state = putStr (D.showDatabase $ S.current state) >> newline
outPossibleTags = putStrLn . intercalate "\n" . sort . elems . S.possibleTags

cdPlus  ts stateRef = modifyIORef stateRef (S.addTags ts)
cdMinus ts stateRef = modifyIORef stateRef (S.removeTags ts)

editInVim stateRef = do
  state <- readIORef stateRef
  let contents = D.showDatabase (S.currentWithFullPaths state)
  writeFile tempPath contents
  modifyIORef stateRef S.removeCurrent
  runVim tempPath
  newContents <- readFile tempPath
  let statePart = D.parseDatabase newContents
  modifyIORef stateRef (S.insert statePart)        

-- Read/Write state
dbPath = ".encyclo"
tempPath = ".encyclotemp"

open :: IO S.State
open = do
  input <- readFile dbPath
  return $ S.fromDatabase $ D.parseDatabase input

save :: S.State -> IO ()
save state = 
  writeFile dbPath (D.showDatabase (S.full state))   

runVim path = do 
  process <- runCommand $ "vim " ++ path
  waitForProcess process
  
