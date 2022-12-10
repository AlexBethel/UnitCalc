module Main where

import Control.Monad.Trans.Except (runExceptT)
import Control.Monad.Trans.State (runStateT)
import Lib (VarState, initState, interpret)

main :: IO ()
main = do
  putStrLn "UnitCalc v0.1.0"
  repl initState

-- Run a read-eval-print loop in the language, with the given initial
-- state.
repl :: VarState -> IO ()
repl s = do
  (result, nextState) <- runStateT (runExceptT interpret) s
  case result of
    Left err -> putStrLn $ "Unhandled exception: " ++ show err
    Right () -> pure ()
  repl nextState
