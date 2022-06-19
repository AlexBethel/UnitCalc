module Main where

import Control.Monad.State (runStateT)
import Lib (State, initState, interpret)

main :: IO ()
main = do
  putStrLn "UnitCalc v0.1.0"
  repl initState

-- Run a read-eval-print loop in the language, with the given initial
-- state.
repl :: State -> IO ()
repl s = do
  ((), nextState) <- runStateT interpret s
  repl nextState
