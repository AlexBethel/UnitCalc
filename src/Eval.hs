-- Expression evaluation functions.
{-# LANGUAGE RankNTypes #-}

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
import Control.Monad.Trans.Except (ExceptT, catchE, throwE)
import Data.IORef (IORef, readIORef, writeIORef)
import Data.Map.Strict (Map, empty, lookup)
import Data.Ratio (denominator, numerator)
import GHC.Float (powerDouble)
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

instance Show Value where
  show (IntVal i) = show i
  show (DoubleVal d) = show d
  show (StringVal s) = show s
  show (BoolVal b) = show b
  show (LambdaVal p e) = "<lambda>"

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

-- Catch an exception in the Interp monad.
interpCatch :: Interp a -> Interp (Either Value a)
interpCatch from = catchE (Right <$> from) (pure . Left)

-- Error for unimplemented functionality.
unimplemented :: Interp a
unimplemented = throwE (StringVal "not yet implemented")

-- Let's define that lambdas are never equal to one another; this
-- makes a lot of things much simpler, and doesn't often have any real
-- impact on the power of the language.
instance Eq Value where
  (IntVal l) == (IntVal r) = l == r
  (DoubleVal l) == (DoubleVal r) = l == r
  (IntVal l) == (DoubleVal r) = fromInteger l == r
  (DoubleVal l) == (IntVal r) = l == fromInteger r
  (StringVal l) == (StringVal r) = l == r
  (BoolVal l) == (BoolVal r) = l == r
  (LambdaVal _ _) == (LambdaVal _ _) = False
  _ == _ = False

-- Conversions to doubles. This is used to promote values for doing
-- arithmetic.
toDouble :: Value -> Interp Double
toDouble (IntVal n) = pure $ fromInteger n
toDouble (DoubleVal n) = pure n
toDouble (BoolVal b) = unimplemented
toDouble (StringVal s) = unimplemented
toDouble (LambdaVal pat body) = unimplemented

-- Evaluates an expression to get a value.
evalExpression :: Expression -> Interp Value
evalExpression e = case e of
  Add l r -> runNumOp (+) l r
  Sub l r -> runNumOp (-) l r
  Mul l r -> runNumOp (*) l r
  Div l r -> do
    lv <- toDouble =<< evalExpression l
    rv <- toDouble =<< evalExpression r
    if rv == 0
      then throwE $ StringVal "divide by zero"
      else return $ DoubleVal (lv / rv)
  Mod l r -> do
    lv <- evalExpression l
    rv <- evalExpression r
    case (lv, rv) of
      (IntVal l, IntVal r) -> pure $ IntVal (l `mod` r)
      _ -> do
        l <- toDouble lv
        r <- toDouble rv
        pure $ DoubleVal (l - fromInteger (truncate (l / r)) * r)
  Pow l r -> do
    lv <- evalExpression l
    rv <- evalExpression r
    valPow lv rv
  Equ l r -> do
    lv <- evalExpression l
    rv <- evalExpression r
    pure $ BoolVal (lv == rv)
  Neq l r -> do
    lv <- evalExpression l
    rv <- evalExpression r
    pure $ BoolVal (lv /= rv)
  Lt l r -> unimplemented
  Leq l r -> unimplemented
  Gt l r -> unimplemented
  Geq l r -> unimplemented
  And l r -> unimplemented
  Or l r -> unimplemented
  Minus e -> unimplemented
  Plus e -> evalExpression e
  Call fn arg -> callFn fn arg
  Tuple [elem] -> evalExpression elem
  Tuple elems -> unimplemented
  Variable name -> interpGetVar name
  Literal lit -> case lit of
    StrLiteral x -> pure (StringVal x)
    IntLiteral x -> pure (IntVal x)
    DecLiteral x -> pure (DoubleVal x)
  Lambda pat body -> unimplemented
  IfElse condition thenClause elseClause -> unimplemented
  Sequence l r -> unimplemented

-- Evaluate a pure binary operation in the `StateT VarState IO` monad.
numOp ::
  (forall a. Num a => (a -> a -> a)) ->
  Value ->
  Value ->
  Interp Value
numOp fn l r =
  case (l, r) of
    (IntVal ln, IntVal rn) -> pure $ IntVal (fn ln rn)
    (IntVal ln, DoubleVal rn) -> pure $ DoubleVal (fn (fromInteger ln) rn)
    (DoubleVal ln, IntVal rn) -> pure $ DoubleVal (fn ln (fromInteger rn))
    (DoubleVal ln, DoubleVal rn) -> pure $ DoubleVal (fn ln rn)
    _ ->
      throwE
        ( StringVal $
            "Unsupported operands: "
              ++ show l
              ++ ", "
              ++ show r
        )

runNumOp ::
  (forall a. Num a => (a -> a -> a)) ->
  Expression ->
  Expression ->
  Interp Value
runNumOp fn l r = do
  lv <- evalExpression l
  rv <- evalExpression r
  numOp fn lv rv

valPow :: Value -> Value -> Interp Value
valPow l r | l == IntVal 0 && r == IntVal 0 = throwE (StringVal "undefined form 0 ^ 0")
valPow (IntVal l) (IntVal r) = pure $ IntVal (l ^ r)
valPow (DoubleVal l) (IntVal r) = pure $ DoubleVal (l ^ r)
valPow l r = do
  l <- toDouble l
  r <- toDouble r
  let res = powerDouble l r
   in if isNaN res
        then throwE (StringVal "complex numbers unimplemented")
        else pure $ DoubleVal (powerDouble l r)

callFn :: Expression -> Expression -> Interp Value
callFn l r = do
  fn <- evalExpression l
  arg <- evalExpression r
  case fn of
    IntVal _ -> unimplemented
    DoubleVal _ -> unimplemented
    StringVal _ -> unimplemented
    BoolVal _ -> unimplemented
    LambdaVal _ _ -> unimplemented
