module Lib
  ( VarState,
    initState,
    interpret,
    parseExpression,
  )
where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.State (StateT, runStateT)
import Eval (Interp, VarState, evalExpression, initState)
import Parser
import System.IO (hFlush, stdout)
import Text.Parsec (eof, runParser)
import Control.Monad.Trans.Except (runExceptT)

interpret :: Interp ()
interpret = do
  text <- liftIO $ do
    putStr "> "
    hFlush stdout
    getLine
  let parsed = runParser (parseExpression <* eof) () "<stdin>" text
  liftIO $ print parsed
  case parsed of
    Left err -> liftIO $ do
      print err
    Right exp -> do
      val <- evalExpression exp
      liftIO $ print val

repl :: VarState -> IO ()
repl s = do
  (result, nextState) <- runStateT (runExceptT interpret) s
  case result of
    Left err -> print err
    Right () -> pure ()
  repl nextState
