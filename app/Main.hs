module Main where

import Control.Monad.State (runStateT)
import Lib (VarState, initState, interpret)

main :: IO ()
main = do
  putStrLn "UnitCalc v0.1.0"
  repl initState

-- Run a read-eval-print loop in the language, with the given initial
-- state.
repl :: VarState -> IO ()
repl s = do
  ((), nextState) <- runStateT interpret s
  repl nextState
