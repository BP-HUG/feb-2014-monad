-- Krisztian Pinter, 2014
import Control.Monad.State
import Data.Char
import Data.List
import Data.Maybe

class REPLState s where
  setInput :: String -> s -> s
  getOutput :: s -> String
  running :: s -> Bool

repl :: (REPLState s) => (s -> s) -> StateT s IO ()
repl f = do
  i <- liftIO getLine
  modify $ f.setInput i
  st <- get
  liftIO $ putStrLn $ getOutput st
  when (running st) $ repl f

runRepl :: (REPLState s) => (s -> s) -> s -> IO ()
runRepl f s = evalStateT (repl f) s

-- Example -------------------

data ExampleState = ES  { r :: Bool
                        , o :: String
                        , i :: String

                        , names :: [String]
                        }

exampleStart :: ExampleState
exampleStart = ES { r = True, o = "", i = "", names = []}

instance REPLState ExampleState where
  setInput i s = s { i = i }
  getOutput = o
  running = r

getInput = i
setOutput o s = s { o = o }

exampleFun :: ExampleState -> ExampleState
exampleFun st = do
  let i = getInput st
  parse i st

parse :: String -> ExampleState -> ExampleState
parse i st
  | "exit" == i  = setOutput "Exiting" st{ r = False }
  | pref "name " = setOutput (i ++ " saved") st{ names = (strip "name ":names st) }
  | pref "hi "   = setOutput (if   strip "hi " `elem` names st
                              then "hi " ++ (strip "hi ") ++ "!"
                              else "i don't know you") st
  | otherwise    = setOutput ((map toUpper i) ++ "!!!") st
  where
    pref str  = isPrefixOf str i
    strip str = fromJust $ stripPrefix str i  

runExample = runRepl exampleFun exampleStart