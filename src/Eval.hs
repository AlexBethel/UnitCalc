
-- Expression evaluation functions.

-- TODO: Deal with all `undefined`s here; the final program should
-- contain zero.
module Eval
  ( evalExpression,
    Value,
    VarState (VarState, variables),
    initState,
  )
where

import Control.Monad.State
  ( MonadState (get, put, state),
    StateT (runStateT),
  )
import Data.Map.Strict (Map, empty)
import Data.Ratio (denominator, numerator)
import Parser
  ( Expression
      ( Add,
        And,
        Call,
        Div,
        Equ,
        Geq,
        Gt,
        IfElse,
        Lambda,
        Leq,
        Literal,
        Lt,
        Minus,
        Mod,
        Mul,
        Neq,
        Or,
        Plus,
        Pow,
        Sequence,
        Sub,
        Tuple,
        Variable
      ),
    Literal
      ( DecLiteral,
        IntLiteral,
        StrLiteral
      ),
    Pattern
      ( TuplePat,
        VariablePat
      ),
  )

-- The state of the interpreter between any two statements.
newtype VarState = VarState {variables :: Map String Value}

-- The initial state of the interpreter, immediately after startup.
initState :: VarState
initState = VarState {variables = empty}

-- A value, produced from evaluating an expression.
data Value
  = -- Arbitrary-precision integers.
    IntVal Integer
  | -- Decimal values.
    DoubleVal Double
  | -- Strings of characters.
    StringVal String
  | -- Boolean values.
    BoolVal Bool
  | -- Lambda functions. The first argument is the list of bound
    -- parameters, and the second is the body of the function to be
    -- evaluated when called.
    LambdaVal Pattern Expression
  deriving (Show)

-- Let's define that lambdas are never equal to one another; this
-- makes a lot of things much simpler, and doesn't often have any real
-- impact on the power of the language.
instance Eq Value where
  (IntVal l) == (IntVal r) = l == r
  (DoubleVal l) == (DoubleVal r) = l == r
  (StringVal l) == (StringVal r) = l == r
  (BoolVal l) == (BoolVal r) = l == r
  (LambdaVal _ _) == (LambdaVal _ _) = False
  _ == _ = False

-- Conversions to doubles. This is used to promote values for doing
-- arithmetic.
toDouble :: Value -> Double
toDouble (IntVal n) = fromInteger n
toDouble (DoubleVal n) = n
toDouble (BoolVal b) = undefined
toDouble (StringVal s) = undefined
toDouble (LambdaVal pat body) = undefined

-- Evaluates an expression, which may have side effects on the program
-- state (hence the StateT), or arbitrary side effects on the real
-- world (hence the IO).
evalExpression :: Expression -> StateT VarState IO Value
evalExpression e = case e of
  Add l r -> undefined
  Sub l r -> undefined
  Mul l r -> undefined
  Div l r -> undefined
  Mod l r -> undefined
  Pow l r -> undefined
  Equ l r -> undefined
  Neq l r -> undefined
  Lt l r -> undefined
  Leq l r -> undefined
  Gt l r -> undefined
  Geq l r -> undefined
  And l r -> undefined
  Or l r -> undefined
  Minus e -> undefined
  Plus e -> evalExpression e
  Call fn arg -> callFn fn arg
  Tuple elems -> undefined
  Variable name -> undefined
  Literal lit -> case lit of
    StrLiteral x -> pure (StringVal x)
    IntLiteral x -> pure (IntVal x)
    DecLiteral x -> pure (DoubleVal x)
  Lambda pat body -> undefined
  IfElse condition thenClause elseClause -> undefined
  Sequence l r -> undefined

-- Evaluate a pure binary operation in the `StateT VarState IO` monad.
evalBinop ::
  (Value -> Value -> Value) ->
  Expression ->
  Expression ->
  StateT VarState IO Value
evalBinop op l r = do
  l <- evalExpression l
  r <- evalExpression r
  pure $ op l r

callFn :: Expression -> Expression -> StateT VarState IO Value
callFn l r = do
  fn <- evalExpression l
  arg <- evalExpression r
  case fn of
    IntVal _ -> undefined
    DoubleVal _ -> undefined
    StringVal _ -> undefined
    BoolVal _ -> undefined
    LambdaVal _ _ -> undefined
