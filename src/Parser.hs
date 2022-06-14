module Parser
  ( parseCommand,
    Command,
    parseExpression,
  )
where

import Control.Applicative (liftA2)
import Control.Monad (void)
import Data.Functor (($>))
import Text.Parsec
  ( Parsec,
    alphaNum,
    anyChar,
    between,
    char,
    choice,
    digit,
    letter,
    lookAhead,
    many,
    many1,
    noneOf,
    sepBy,
    skipMany,
    space,
    string,
    try,
    unexpected,
    (<?>),
    (<|>),
  )
import Text.Parsec.String (Parser)
import Text.Read (Lexeme (Char), readMaybe)

parseCommand :: String -> String
parseCommand = id

data Command
  = -- Evaluate an expression and print the result.
    EvalCommand Expression
  | -- Exit the program.
    QuitCommand
  deriving (Show)

-- An expression, represented as an abstract syntax tree.
data Expression
  = -- Sum of two expressions.
    Add Expression Expression
  | -- Difference between two expressions.
    Sub Expression Expression
  | -- Product of two expressions.
    Mul Expression Expression
  | -- Quotient of two expressions.
    Div Expression Expression
  | -- Remainder of two expressions' division.
    Mod Expression Expression
  | -- One expression raised to the power of another.
    Pow Expression Expression
  | -- Unary minus operator.
    Minus Expression
  | -- Unary plus operator, which is equivalent to absolute value.
    Plus Expression
  | -- Call one expression (whose type must be a function) on an
    -- argument.
    Call Expression Expression
  | -- Combine the results of several expressions into a tuple.
    Tuple [Expression]
  | -- A variable reference; the string is the name of the variable.
    Variable String
  | -- A literal value.
    Literal Literal
  deriving (Show)

data Literal
  = -- Literal string enclosed in double quotes.
    StrLiteral String
  | -- Literal integer, which is a string of one or more digits.
    IntLiteral Int
  | -- TODO: Should that be a double? It might be better to represent
    -- the individual digits.
    DecLiteral Double
  deriving (Show)

-- Basic Parsec setup
lexeme :: Parser a -> Parser a
lexeme = (<* skipMany space)

word :: String -> Parser String
word = lexeme . string

-- Parse a string literal.
parseString :: Parser Literal
parseString =
  lexeme
    ( between
        (char '"')
        (char '"')
        (StrLiteral <$> many stringChar)
        <?> "string literal"
    )
  where
    stringChar = noneOf ['\\', '\"'] <|> char '\\' *> (backslash <$> anyChar)
    backslash c = case c of
      'n' -> '\n'
      't' -> '\t'
      c -> c

-- Parse a number, i.e., either an integer or a decimal.
parseNumber :: Parser Literal
parseNumber =
  lexeme
    ( do
        leading <- many1 digit
        choice
          [ do
              _ <- char '.'
              trailing <- many1 digit
              pure $ DecLiteral (read (leading ++ "." ++ trailing)),
            pure $ IntLiteral (read leading)
          ]
    )
    <?> "number literal"

-- Parse a literal value in an expression.
parseLiteral :: Parser Expression
parseLiteral =
  Literal
    <$> choice
      [ parseString,
        parseNumber
      ]

-- Parse a variable name.
parseVariable :: Parser Expression
parseVariable = Variable <$> lexeme (liftA2 (:) first rest) <?> "variable name"
  where
    first = letter
    rest = many alphaNum

-- An operator, i.e., a single character optionally surrounded by
-- whitespace.
op :: Char -> Parser ()
op = lexeme . void . char

-- Parse a tuple of zero or more expressions.
parseTuple :: Parser Expression
parseTuple =
  Tuple
    <$> between
      (op '(')
      (op ')')
      (parseExpression `sepBy` op ',')

data Assoc = AssocLeft | AssocRight

-- Parse a list of infix operations between elements. All the infix
-- operations are of the same precedence level and associativity.
parseInfixes ::
  -- | Parsers for the operators, which return the operator mapping: a
  -- | function that takes two expressions (left- and right-hand side,
  -- | respectively) and returns a new expression.
  [Parser (Expression -> Expression -> Expression)] ->
  -- | Associativity of the operation.
  Assoc ->
  -- | The constituent elements in the infix expression.
  Parser Expression ->
  Parser Expression
parseInfixes infixes assoc element = do
  first <- element
  rest <- many ((,) <$> choice infixes <*> element)
  pure $
    ( case assoc of
        AssocLeft -> foldl (\l (operator, r) -> operator l r)
        AssocRight -> foldr (\(operator, r) l -> operator l r)
    )
      first
      rest

-- Parse a list of prefixes that can precede an expression.
parsePrefixes ::
  -- | The list of prefixes. Each prefix maps an expression to a new
  -- | expression, so is of type Expression -> Expression.
  [Parser (Expression -> Expression)] ->
  -- | The expression parser that can be preceded by prefixes.
  Parser Expression ->
  Parser Expression
parsePrefixes prefixes element = do
  p <- many (choice prefixes)
  e <- element
  pure $ foldl (\expression mapping -> mapping expression) e p

-- The base expression parser, which includes literals (1, 2.5,
-- "hello"), variables (print, x), and tuples ((1, 2), ("hello",
-- xyz)); tuples are used to implement parentheses so arbitrary
-- subexpressions can be at any position in an expression.
expBase :: Parser Expression
expBase = parseLiteral <|> parseVariable <|> parseTuple

-- Parser for a b (function application).
expCall :: Parser Expression -> Parser Expression
expCall =
  parseInfixes
    [ Call <$ pure ()
    ]
    AssocLeft

-- Parser for +a and -a.
expPlusMinus :: Parser Expression -> Parser Expression
expPlusMinus =
  parsePrefixes
    [ Plus <$ op '+',
      Minus <$ op '-'
    ]

-- Parser for a + b and a - b.
expAddSub :: Parser Expression -> Parser Expression
expAddSub =
  parseInfixes
    [ Add <$ op '+',
      Sub <$ op '-'
    ]
    AssocLeft

-- Parser for a * b, a / b and a % b.
expMulDiv :: Parser Expression -> Parser Expression
expMulDiv =
  parseInfixes
    [ Mul <$ op '*',
      Div <$ op '/',
      Mod <$ op '%'
    ]
    AssocLeft

-- Parser for a^b.
expPow :: Parser Expression -> Parser Expression
expPow =
  parseInfixes
    [ Pow <$ op '^'
    ]
    AssocRight

-- The main expression parser.
parseExpression :: Parser Expression
parseExpression =
  -- Fold all the smaller expression parsers into one big parser.
  foldl
    (\e p -> p e)
    expBase
    [ expCall,
      expPow,
      expPlusMinus,
      expMulDiv,
      expAddSub
    ]
