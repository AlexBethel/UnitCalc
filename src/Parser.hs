module Parser
  ( parseCommand,
    Command,
    parseExpression,
    Expression
      ( Add,
        Sub,
        Mul,
        Div,
        Mod,
        Pow,
        Equ,
        Neq,
        Lt,
        Leq,
        Gt,
        Geq,
        And,
        Or,
        Minus,
        Plus,
        Call,
        Tuple,
        Variable,
        Literal,
        Lambda,
        IfElse,
        Sequence
      ),
    Literal
      ( StrLiteral,
        IntLiteral,
        DecLiteral
      ),
    Pattern
      ( VariablePat,
        TuplePat
      ),
  )
where

import Control.Monad (void)
import Text.Parsec
  ( alphaNum,
    anyChar,
    between,
    char,
    choice,
    digit,
    letter,
    many,
    many1,
    noneOf,
    optionMaybe,
    sepBy,
    skipMany,
    space,
    string,
    try,
    (<?>),
    (<|>),
  )
import Text.Parsec.String (Parser)

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
  | -- Equals comparison.
    Equ Expression Expression
  | -- Not equal comparison.
    Neq Expression Expression
  | -- Less than comparison.
    Lt Expression Expression
  | -- Less than or equals comparison.
    Leq Expression Expression
  | -- Greater than comparison.
    Gt Expression Expression
  | -- Greater than or equals comparison.
    Geq Expression Expression
  | -- Boolean AND operator.
    And Expression Expression
  | -- Boolean OR operator.
    Or Expression Expression
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
  | -- A lambda that takes a list of arguments and returns an
    -- expression.
    Lambda Pattern Expression
  | -- Evaluate an expression if another expression is truthy;
    -- otherwise optionally evaluate another (else) condition.
    IfElse Expression Expression (Maybe Expression)
  | -- Evaluate an expression, then evaluate another.
    Sequence Expression (Maybe Expression)
  deriving (Show)

data Literal
  = -- Literal string enclosed in double quotes.
    StrLiteral String
  | -- Literal integer, which is a string of one or more digits.
    IntLiteral Integer
  | -- TODO: Should that be a double? It might be better to represent
    -- the individual digits.
    DecLiteral Double
  deriving (Show)

-- Patterns for binding in lambdas and assignments.
data Pattern
  = VariablePat String
  | TuplePat [Pattern]
  deriving (Show)

reservedWords :: [String]
reservedWords =
  [ "if",
    "then",
    "else"
  ]

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
      'r' -> '\r'
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
              trailing <- char '.' *> many1 digit
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
parseVariableName :: Parser String
parseVariableName =
  lexeme
    -- We use `try` here to prevent reading reserved words.
    ( try $ do
        -- Variable names have to start with a letter.
        word <- (:) <$> letter <*> many alphaNum
        if word `elem` reservedWords
          then fail ("got reserved word `" ++ word ++ "`")
          else pure word
    )
    <?> "variable name"

-- Parse a variable literal.
parseVariable :: Parser Expression
parseVariable = Variable <$> parseVariableName

-- An operator, i.e., a single character optionally surrounded by
-- whitespace.
op :: Char -> Parser ()
op = lexeme . void . char

-- Parse a tuple of zero or more expressions.
parseTuple :: Parser Expression
parseTuple =
  ( Tuple
      <$> between
        (op '(')
        (op ')')
        (parseExpression `sepBy` op ',')
  )
    <?> "parentheses"

-- Parse an `if-then-else` expression.
parseIfElse :: Parser Expression
parseIfElse =
  ( IfElse
      <$> (word "if" *> parseExpression)
      <*> (word "then" *> parseExpression)
      <*> optionMaybe (word "else" *> parseExpression)
  )
    <?> "if expression"

-- Parse a lambda expression.
parseLambda :: Parser Expression
parseLambda =
  ( Lambda
      <$> (op '\\' *> parsePattern)
      <*> (word "->" *> parseExpression)
  )
    <?> "lambda expression"

parseVariablePat :: Parser Pattern
parseVariablePat = VariablePat <$> parseVariableName

parseTuplePat :: Parser Pattern
parseTuplePat =
  ( TuplePat
      <$> between
        (op '(')
        (op ')')
        (parsePattern `sepBy` op ',')
  )
    <?> "parentheses"

parsePattern :: Parser Pattern
parsePattern = parseVariablePat <|> parseTuplePat

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
        -- BUG: AssocRight generates wildly incorrect results
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
expBase =
  choice
    [ parseLiteral,
      parseTuple,
      parseVariable,
      parseIfElse,
      parseLambda
    ]

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

-- Parser for comparisons.
expCompare :: Parser Expression -> Parser Expression
expCompare =
  parseInfixes
    ( map
        -- Some expressions are substrings of each other, meaning we
        -- need to look ahead by one character to get this right.
        try
        [ Equ <$ word "==",
          Neq <$ word "!=",
          Lt <$ word "<",
          Leq <$ word "<=",
          Gt <$ word ">",
          Geq <$ word ">="
        ]
    )
    AssocLeft

-- Parser for binary boolean operators.
expAndOr :: Parser Expression -> Parser Expression
expAndOr =
  parseInfixes
    [ And <$ op '&',
      Or <$ op '|'
    ]
    AssocLeft

-- Parser for a; b.
expSequence :: Parser Expression -> Parser Expression
expSequence exp = do
  lhs <- exp
  semi <- optionMaybe $ op ';'
  case semi of
    Just _ -> do
      rhs <- optionMaybe $ expSequence exp
      pure $ Sequence lhs rhs
    Nothing -> do
      pure lhs

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
      expAddSub,
      expCompare,
      expAndOr,
      expSequence
    ]
