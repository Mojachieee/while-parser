



{-
While Programming language BNF
http://studentnet.cs.manchester.ac.uk/ugt/2012/COMP11212/lecture03.pdf

x,y - Program Variables
n - Number literals

S - Statements
S ::= x:a
    | skip
    | S1; S2
    | if P then s1 else S2
    | while P do S

a - Arithmetic Expresions
a ::= x
    | n
    | a1 oPa a2
oPa - Arithmetic operations
oPa ::= + | - | * | /

P - Boolean Predicates
P ::= true
    | false
    | not P
    | P1 oPb P2
    | a1 oPr a2

oPb ::= and | or
oPr ::= < | <= | = | > | >=
-}
-- While Language implementation using applicative parsing (as oppose to monadic parsing)
module While where
import Control.Monad (void)
import Text.Megaparsec
import Text.Megaparsec.Expr
import Text.Megaparsec.String   -- input stream is of type â€˜Stringâ€™
import qualified Text.Megaparsec.Lexer as Lexer

{-
  Useful functions

  (<|>) :: Alternative f => f a -> f a -> f a
    Or statement, without caring for associativity

  (<$>) :: Functor f => (a -> b) -> f a -> f b
  Simply Fmap
  Fmap maps a funtion over a number of As to give Bs

  (<$) :: Functor f => a -> f b -> f a
  - Replace all locations in the input with the same value.

  (<*>) :: Applicative f => f (a -> b) -> f a -> f b
  Apply many functions to many values

  (*>) :: f a -> f b -> f b
  Sequence of actions, discard first return

  (<*) :: f a -> f b -> f a
  Sequence of actions, discard second return

 -}

-- Use parseTest p string           to test parse p on the string
parseFromFile :: Parsec e String a -> String -> IO (Either (ParseError Char e) a)
parseFromFile p file = runParser p file <$> readFile file

        -- LEXER BEGINS --

whiteSpace :: Parser ()   -- Deals with whiteSpace
whiteSpace = Lexer.space (void spaceChar) lineCmnt blockCmnt
          where lineCmnt  = Lexer.skipLineComment "//"
                blockCmnt = Lexer.skipBlockComment "/*" "*/"

lexeme :: Parser a -> Parser a    -- Consume white space after each lexeme
lexeme = Lexer.lexeme whiteSpace

symbol :: String -> Parser String     -- Parses a given string and the whitespace after it
symbol = Lexer.symbol whiteSpace

parens :: Parser a -> Parser a        -- Parses stuff between parenthesis
parens = between (symbol  "(") (symbol ")")

semi :: Parser String              -- Parses a semi-colon
semi = symbol ";"

integer :: Parser Integer             -- Parses an Integer
integer = lexeme Lexer.integer

kword :: String -> Parser ()      -- Checks a parsed keyword isn't a prefix of an identifier
kword w = string w *> notFollowedBy alphaNumChar *> whiteSpace

keywords :: [String]          -- All the reserved keywords
keywords = ["if", "then", "else", "while", "do", "skip", "true", "false", "not", "and", "or"]

identifier :: Parser String
identifier = (lexeme . try) (p >>= check)
--  Same as (lexeme (try (p >>= check)))
  where
    p       = (:) <$> letterChar <*> many alphaNumChar
    -- Same as     p       = fmap (:) letterChar <*> many alphaNumChar
    check x = if x `elem` keywords
                then fail $ "keyword " ++ show x ++ " cannot be used as an identifier"
                else return x


                -- LEXER ENDS --


                -- PARSER BEGINS (and datatypes) --

-- Statements
data Stat = SEQ [Stat]
       | ASSIGN String A
       | IF B Stat Stat
       | WHILE B Stat
       | SKIP
       deriving (Show)

-- Boolean Expressions
data B = BoolConst Bool
      |  NOT B
      |  BBinary OPb B B     -- For example true && false
      |  RBinary OPr A A
      deriving (Show)

-- Arithmetic Expressions
data A = Var String
       | IntConst Integer
       | Neg A
       | ABinary OPa A A
       deriving (Show)

-- Boolean Operators
data OPb = AND | OR deriving (Show)
--Relational Operators
data OPr = Greater | Lesser | GreaterEquals | LesserEquals deriving (Show)

--Arithmetic Operators
data OPa = Add
         | Subtract
         | Multiply
         | Divide
         deriving (Show)


whileParser :: Parser Stat
whileParser = whiteSpace *> stat <* eof   -- get rid of initial whiteSpace

stat :: Parser Stat
stat = parens stat <|> statSeq

statSeq :: Parser Stat
statSeq = f <$> sepBy1 stat' semi
      where f l = if length l == 1 then head l else SEQ l   -- If there's only 1 statement return it

stat' :: Parser Stat    -- Parsers all possible statements
stat' = ifStat <|> whileStat <|> skipStat <|> assignStat

ifStat :: Parser Stat   -- Parses an if statement
ifStat = IF <$ kword "if" <*> b <* kword "then" <*> stat <* kword "else" <*> stat

  -- Equivalent with do notation
  -- do kword "if"
  --    cond <- b
  --    kword "then"
  --    stat1 <- stat
  --    kword "else"
  --    stat2 <- stat
  --    return $ IF cond stat1 stat2


whileStat :: Parser Stat    -- Parses a while statement
whileStat =
  do kword "while"
     cond <- b
     kword "do"
     stat1 <- stat
     return $ WHILE cond stat1

 -- Equivalent definition for while
 -- WHILE <$ kword "while" <*> b <* kword "do" <*> stat

 -- $ is for avoiding parentheses, it give precedence to stuff after it

skipStat :: Parser Stat     -- Parses a skip statement
skipStat = SKIP <$ kword "skip"

assignStat :: Parser Stat
assignStat =
  do var <- identifier
     void $ symbol ":="
     expr <- a
     return $ ASSIGN var expr

-- Alternate way
b' :: Parser B
b' = parens b'
    <|> BoolConst True <$ kword "True"
    <|> BoolConst False <$ kword "False"
    <|> NOT <$ kword "not" <*> b
    <|> BBinary AND <$> b <* kword "and" <*> b
    <|> BBinary OR <$> b <* kword "or" <*> b
    <|> oPr

    -- Do notation for AND
    -- do b1 <- b
    --        kword "and"
    --        b2 <- b
    --        return $ BBinary AND b1 b2


b :: Parser B
b = makeExprParser bTerm bOperators

a :: Parser A
a = makeExprParser aTerm aOperators

bOperators :: [[Operator Parser B]]
bOperators = [ [ Prefix (kword "not" *> pure NOT) ] ,
               [ InfixL (kword "and" *> pure (BBinary AND)) ] ,
               [ InfixL (kword "or" *> pure (BBinary OR)) ]
             ]

aOperators :: [[Operator Parser A]]
aOperators = [ [ Prefix (symbol "-" *> pure Neg) ] ,
               [ InfixL (symbol "*" *> pure (ABinary Multiply)) ] ,
               [ InfixL (symbol "/" *> pure (ABinary Divide)) ] ,
               [ InfixL (symbol "+" *> pure (ABinary Add)) ] ,
               [ InfixL (symbol "-" *> pure (ABinary Subtract)) ]
             ]

bTerm :: Parser B
bTerm = parens b
     <|> (kword "true") *> pure (BoolConst True)
     <|> (kword "false") *> pure (BoolConst False)
     <|> oPr

aTerm :: Parser A
aTerm = parens a
     <|> Var     <$>  identifier
     <|> IntConst <$> integer


oPr :: Parser B
oPr = do
  a1 <- a
  op <- relation
  a2 <- a
  return $ RBinary op a1 a2

relation :: Parser OPr
relation =  (symbol ">=" *> pure GreaterEquals)
        <|> (symbol "<=" *> pure LesserEquals)
        <|> (symbol ">" *> pure Greater)
        <|> (symbol "<" *> pure Lesser)