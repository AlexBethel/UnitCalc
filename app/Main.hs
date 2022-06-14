module Main where

import Data.Foldable (forM_)
import Lib (State, initState, interpret, parseExpression)
import Text.Parsec (runParser, eof)
import System.IO (hFlush, stdout)

main :: IO ()
main = do
  putStr "Text to parse: "
  hFlush stdout
  text <- getLine
  let result = runParser (parseExpression <* eof) () "<stdin>" text
  print result
  main

-- Run a read-eval-print loop in the language, with the given initial
-- state.
repl :: State -> IO ()
repl s = do
  nextState <- interpret s
  forM_ nextState repl
