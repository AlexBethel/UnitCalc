module Lib
  ( State,
    initState,
    interpret,
    parseExpression,
  )
where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.State (StateT)
import Eval (State, evalExpression, initState)
import Parser
import System.IO (hFlush, stdout)
import Text.Parsec (eof, runParser)

interpret :: StateT State IO ()
interpret = do
  text <- liftIO $ do
    putStr "> "
    hFlush stdout
    getLine
  let parsed = runParser (parseExpression <* eof) () "<stdin>" text
  case parsed of
    Left err -> liftIO $ do
      print err
    Right exp -> do
      val <- evalExpression exp
      liftIO $ print val
