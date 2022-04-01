module Lib
  ( State,
    initState,
    interpret,
  )
where

type State = String

initState :: State
initState = ""

interpret :: State -> IO (Maybe State)
interpret state = do
  putStrLn state
  Just <$> getLine
