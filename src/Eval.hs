-- Expression evaluation functions.

-- TODO: Deal with all `undefined`s here; the final program should
-- contain zero.
module Eval
  ( evalExpression,
    Value,
    State (State, variables),
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
newtype State = State {variables :: Map String Int}

-- The initial state of the interpreter, immediately after startup.
initState :: State
initState = State {variables = empty}

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

-- We'll probably end up abandoning this, since `Num` requires that
-- all operations are infallible, whereas in our program some
-- operations are fallible.
instance Num Value where
  (IntVal l) + (IntVal r) = IntVal (l + r)
  l + r = DoubleVal (toDouble l + toDouble r)

  (IntVal l) * (IntVal r) = IntVal (l * r)
  l * r = DoubleVal (toDouble l * toDouble r)

  abs (IntVal n) = IntVal (abs n)
  abs (DoubleVal n) = DoubleVal (abs n)
  abs (BoolVal b) = undefined
  abs (StringVal s) = undefined
  abs (LambdaVal pat body) = undefined

  signum (IntVal n) = IntVal (signum n)
  signum (DoubleVal n) = DoubleVal (signum n)
  signum (BoolVal b) = undefined
  signum (StringVal s) = undefined
  signum (LambdaVal pat body) = undefined

  fromInteger = IntVal

  negate (IntVal n) = IntVal (-n)
  negate (DoubleVal n) = DoubleVal (-n)
  negate (BoolVal b) = undefined
  negate (StringVal s) = undefined
  negate (LambdaVal pat body) = LambdaVal pat (Minus body)

-- See comment on `instance Num Value`
instance Fractional Value where
  fromRational rat = fromInteger (numerator rat) / fromInteger (denominator rat)

  recip (IntVal n) = DoubleVal (recip (fromInteger n))
  recip (DoubleVal n) = DoubleVal (recip n)
  recip (BoolVal b) = undefined
  recip (StringVal s) = undefined
  recip (LambdaVal pat body) = undefined

-- Evaluates an expression, which may have side effects on the program
-- state (hence the StateT), or arbitrary side effects on the real
-- world (hence the IO).
evalExpression :: Expression -> StateT State IO Value
evalExpression e = case e of
  Add l r -> evalBinop (+) l r
  Sub l r -> evalBinop (-) l r
  Mul l r -> evalBinop (*) l r
  Div l r -> evalBinop (/) l r
  Mod l r -> undefined
  Pow l r -> undefined
  Equ l r -> evalBinop (\l r -> BoolVal $ l == r) l r
  Neq l r -> evalBinop (\l r -> BoolVal $ l /= r) l r
  Lt l r -> undefined
  Leq l r -> undefined
  Gt l r -> undefined
  Geq l r -> undefined
  And l r -> undefined
  Or l r -> undefined
  Minus e -> undefined
  Plus e -> undefined
  Call fn arg -> undefined
  Tuple elems -> undefined
  Variable name -> undefined
  Literal lit -> case lit of
    StrLiteral x -> pure (StringVal x)
    IntLiteral x -> pure (IntVal x)
    DecLiteral x -> pure (DoubleVal x)
  Lambda pat body -> undefined
  IfElse condition thenClause elseClause -> undefined
  Sequence l r -> undefined

-- Evaluate a pure binary operation in the `StateT State IO` monad.
evalBinop ::
  (Value -> Value -> Value) ->
  Expression ->
  Expression ->
  StateT State IO Value
evalBinop op l r = do
  l <- evalExpression l
  r <- evalExpression r
  pure $ op l r
