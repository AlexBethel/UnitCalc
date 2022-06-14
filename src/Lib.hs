module Lib
  ( State,
    initState,
    interpret,
    parseExpression,
  )
where

import Data.Map.Strict (Map, empty)
import Parser

-- The state of the interpreter between any two statements.
newtype State = State {variables :: Map String Int}

-- The initial state of the interpreter, immediately after startup.
initState :: State
initState = State {variables = empty}

interpret :: State -> IO (Maybe State)
interpret state = do
  putStr "hello"
  return (Just state)
