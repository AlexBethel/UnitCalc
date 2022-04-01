module Main where

import Data.Foldable (forM_)
import Lib (State, initState, interpret)

main :: IO ()
main = repl initState

-- Run a read-eval-print loop in the language, with the given initial
-- state.
repl :: State -> IO ()
repl s = do
  nextState <- interpret s
  forM_ nextState repl
