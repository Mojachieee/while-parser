



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
import Prelude hiding (Num)
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

parseFile :: FilePath -> IO () 
parseFile filePath = do
  file <- readFile filePath
  putStrLn $ case parse whileParser filePath file of
      Left err -> parseErrorPretty err
      Right x -> show x

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
keywords = ["if", "then", "else", "while", "do", "skip", "true", "false", "not", "and", "or", "begin", "end", "call", "var", "proc"]

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

type Num = Integer
type Var = String
type Pname = String
type DecV = [(Var,Aexp)]
type DecP = [(Pname, Stm)]

-- Statements
data Stm = Skip
       | Ass Var Aexp
       | Comp Stm Stm
       | If Bexp Stm Stm
       | While Bexp Stm
       | Block DecV DecP Stm
       | Call Pname
       deriving (Show)

-- Boolean Expressions
data Bexp = TRUE | FALSE
      |  Neg Bexp
      |  And Bexp Bexp
      |  Le Aexp Aexp
      |  Eq Aexp Aexp
      |  RBinary OPr Aexp Aexp
      deriving (Show)

-- Arithmetic Expressions
data Aexp = V Var
       | N Num
       | Mult Aexp Aexp
       | Add Aexp Aexp
       | Sub Aexp Aexp
       deriving (Show)

--Relational Operators
data OPr = Greater | Lesser | GreaterEquals deriving (Show)



whileParser :: Parser Stm
whileParser = whiteSpace *> stat <* eof   -- get rid of initial whiteSpace

stat :: Parser Stm
stat = parens stat <|> statSeq <|> parens block

statSeq :: Parser Stm
statSeq = f <$> sepBy1 stat' semi
      where f [] = error "Error"
            f [x] = x
            f (x:xs) = Comp x (f xs)

stat' :: Parser Stm    -- Parsers all possible statements
stat' = ifStat <|> whileStat <|> skipStat <|> assignStat <|> block <|> call

block :: Parser Stm
block =
 do kword "begin"
    variableDecl <- varDecl
    procedureDecl <- procDecl
    stm <- stat
    kword "end"
    return $ Block variableDecl procedureDecl stm

varDecl = many varDecl'

varDecl' =
  do kword "var"
     var <- identifier
     symbol ":="
     exp <- a
     semi
     return $ (var,exp)

procDecl = many procDecl' -- manu says that it can have one or many procedure declarations

procDecl' =
  do kword "proc"
     progName <- identifier
     kword "is"
     stm <- stat'
     semi
     return $ (progName,stm)

call :: Parser Stm -- call p  where p is a string of the name of the program
call =
  do  kword "call"
      progName <- identifier
      return $ Call progName

ifStat :: Parser Stm   -- Parses an if statement
ifStat = If <$ kword "if" <*> b <* kword "then" <*> stat <* kword "else" <*> stat

  -- Equivalent with do notation
  -- do kword "if"
  --    cond <- b
  --    kword "then"
  --    stat1 <- stat
  --    kword "else"
  --    stat2 <- stat
  --    return $ If cond stat1 stat2


whileStat :: Parser Stm    -- Parses a while statement
whileStat =
  do kword "while"
     cond <- b
     kword "do"
     stat1 <- stat
     return $ While cond stat1

 -- Equivalent definition for while
 -- While <$ kword "while" <*> b <* kword "do" <*> stat

 -- $ is for avoiding parentheses, it gives precedence to stuff after it

skipStat :: Parser Stm     -- Parses a skip statement
skipStat = Skip <$ kword "skip"

assignStat :: Parser Stm
assignStat =
  do var <- identifier
     void $ symbol ":="
     expr <- a
     return $ Ass var expr

-- Alternate way
b' :: Parser Bexp
b' = parens b'
    <|> TRUE <$ kword "True"
    <|> FALSE <$ kword "False"
    <|> Neg <$ kword "not" <*> b
    <|> And <$> b <* kword "and" <*> b
    <|> Le <$> a <* kword "<=" <*> a
    <|> oPr

    -- Do notation for And
    -- do b1 <- b
    --        kword "and"
    --        b2 <- b
    --        return $ BBinary And b1 b2


b :: Parser Bexp
b = makeExprParser bTerm bOperators

a :: Parser Aexp
a = makeExprParser aTerm aOperators

bOperators :: [[Operator Parser Bexp]]
bOperators = [ [ Prefix (kword "!" *> pure Neg) ] ,
               [ InfixL (kword "^" *> pure And) ] 
             ]

aOperators :: [[Operator Parser Aexp]]
aOperators = [ 
               [ InfixL (symbol "*" *> pure Mult) ] ,
               [ InfixL (symbol "+" *> pure Add) ] ,
               [ InfixL (symbol "-" *> pure Sub) ]
             ]

bTerm :: Parser Bexp
bTerm = parens b
     <|> (kword "true") *> pure (TRUE)
     <|> (kword "false") *> pure (FALSE)
     <|> Eq <$> a <* symbol "=" <*> a
     <|> Le <$> a <* kword "<=" <*> a


aTerm :: Parser Aexp
aTerm = parens a
     <|> V     <$>  identifier
     <|> N <$> integer


oPr :: Parser Bexp
oPr = do
  a1 <- a
  op <- relation
  a2 <- a
  return $ RBinary op a1 a2

relation :: Parser OPr
relation =  (symbol ">=" *> pure GreaterEquals)
        <|> (symbol ">" *> pure Greater)
        <|> (symbol "<" *> pure Lesser)