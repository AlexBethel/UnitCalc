-- Expression evaluation functions.

-- TODO: Deal with all `undefined`s here; the final program should
-- contain zero.
module Eval
  ( evalExpression,
    Value,
    VarState (VarState, variables),
    initState,
    Interp,
  )
where

import Control.Monad.State
  ( MonadState (get, put, state),
    StateT (runStateT),
  )
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Except (ExceptT, throwE)
import Data.IORef (IORef, readIORef, writeIORef)
import Data.Map.Strict (Map, empty, lookup)
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
import Prelude hiding (lookup)

-- The state of the interpreter between any two statements.
newtype VarState = VarState {variables :: Map String (IORef Value)}

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

-- A computation that might access or change any variables in the
-- interpreter, and that might throw an exception rather than
-- completing.
type Interp a = ExceptT Value (StateT VarState IO) a

-- Runs an IO action in the Interp monad.
interpRun :: IO a -> Interp a
interpRun = lift . lift

-- Gets the current set of all defined variables in scope.
interpVars :: Interp VarState
interpVars = lift get

-- Gets a reference to the variable with the given name. Throws an
-- exception if no variable with the given name exists.
interpGetRef :: String -> Interp (IORef Value)
interpGetRef name = do
  vars <- interpVars
  let refMaybe = lookup name $ variables vars
  case refMaybe of
    Just ref -> pure ref
    Nothing -> throwE $ StringVal ("Undefined variable " ++ name)

-- Gets the value of the variable with the given name.
interpGetVar :: String -> Interp Value
interpGetVar name = do
  ref <- interpGetRef name
  interpRun $ readIORef ref

-- Sets the value of the variable with the given name to the given
-- Value.
interpSetVar :: String -> Value -> Interp ()
interpSetVar name value = do
  ref <- interpGetRef name
  interpRun $ writeIORef ref value

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
evalExpression :: Expression -> Interp Value
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
  Interp Value
evalBinop op l r = do
  l <- evalExpression l
  r <- evalExpression r
  pure $ op l r

callFn :: Expression -> Expression -> Interp Value
callFn l r = do
  fn <- evalExpression l
  arg <- evalExpression r
  case fn of
    IntVal _ -> undefined
    DoubleVal _ -> undefined
    StringVal _ -> undefined
    BoolVal _ -> undefined
    LambdaVal _ _ -> undefined
