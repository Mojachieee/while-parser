




-- While Language implementation 
module While where
import Prelude hiding (Num)
import Control.Monad (void)
import Text.Megaparsec hiding (State, parse)

import Text.Megaparsec.Expr
import Text.Megaparsec.String   -- input stream is of type string
import qualified Text.Megaparsec.Lexer as Lexer
import qualified Text.Megaparsec.Prim as MParse

{-
  Useful functions definitions

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


parse :: String -> Stm
parse str =
    case MParse.parse whileParser "" str of
      Left e  -> error $ show e
      Right r -> r

-- Use parseTest p string           to test parser p on the string
parseFromFile :: Parsec e String a -> String -> IO (Either (ParseError Char e) a)
parseFromFile p file = runParser p file <$> readFile file


-- -- User parseFile "<file name>"     to test the whileParser on a file
-- parseFile :: FilePath -> IO () 
-- parseFile filePath = do
--   file <- readFile filePath
--   putStrLn $ case parse whileParser filePath file of
--       Left err -> parseErrorPretty err
--       Right x -> show x

        -- LEXER BEGINS --

whiteSpace :: Parser ()   -- Parsers whitespace and comments
whiteSpace = Lexer.space (void spaceChar) lineCmnt blockCmnt
          where lineCmnt  = Lexer.skipLineComment "//"
                blockCmnt = Lexer.skipBlockComment "/*" "*/"

lexeme :: Parser a -> Parser a    -- Consume white space after each lexeme
lexeme = Lexer.lexeme whiteSpace

symbol :: String -> Parser String     -- Parses a given string and the whitespace after it
symbol = Lexer.symbol whiteSpace

parens :: Parser a -> Parser a        -- Parses stuff between parentheses
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
whileParser = whiteSpace *> stat <* eof   -- Gets rid of initial whiteSpace

stat :: Parser Stm
stat = parens stat <|> statSeq <|> block <|> parens block

statSeq :: Parser Stm
statSeq = f <$> sepBy1 stat' semi
      where f [] = error "Error"
            f [x] = x
            f (x:xs) = Comp x (f xs)

stat' :: Parser Stm    -- Parses all possible statements
stat' = ifStat <|> whileStat <|> skipStat <|> assignStat <|> parens block <|> block <|> call

block :: Parser Stm
block =
 do kword "begin"
    variableDec <- varDec
    procedureDec <- procDec
    stm <- stat
    kword "end"
    return $ Block variableDec procedureDec stm

varDec :: Parser [(Var,Aexp)]
varDec = many varDec'

varDec' :: Parser (Var,Aexp)
varDec' =
  do kword "var"
     var <- identifier
     symbol ":="
     aexp <- a
     semi
     return $ (var,aexp)

procDec :: Parser [(Pname, Stm)]
procDec = many procDec'

procDec' :: Parser (Pname, Stm)
procDec' =
  do kword "proc"
     procName <- identifier
     kword "is"
     stm <- stat'
     semi
     return $ (procName,stm)

call :: Parser Stm
call =
  do  kword "call"
      procName <- identifier
      return $ Call procName

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


-- Example of how to parse other relations, using an intermediate oPr data constructor (as defined in the Bexp datatype)

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


-- Evaluator Begins

type T = Bool
type Z = Integer
type State = Var -> Z

n_val :: Num -> Z
n_val n = n

a_val :: Aexp-> State -> Z
a_val (N n) s = n_val n
a_val (V x) s = s x
a_val (Add a b) s = (a_val a s) + (a_val b s)
a_val (Mult a b) s = (a_val a s) * (a_val b s)
a_val (Sub a b) s = (a_val a s) - (a_val b s)

b_val :: Bexp -> State -> T
b_val TRUE s       = True
b_val FALSE s      = False 
b_val (Neg b) s    = not (b_val b s)
b_val (And b b') s = (b_val b s) && (b_val b' s)
b_val (Le a a') s  = (a_val a s) <= (a_val a' s)
b_val (Eq a a') s  = (a_val a s) == (a_val a' s)

update :: State -> Z -> Var -> State
update s v x y 
  | x == y  = v
  | otherwise = s y

cond :: (a->T, a->a, a->a) -> (a->a)
cond (p, g, g') s
  | p s = g s
  | otherwise = g' s

fix :: ((State->State)->(State->State))->(State->State)
fix ff = ff (fix ff)

s_ds :: Stm -> State -> State
s_ds (Skip) s       = s
s_ds (Ass x a) s    = update s (a_val a s) x
s_ds (Comp s1 s2) s = ((s_ds s2) . (s_ds s1)) s
s_ds (If b s1 s2) s = (cond (b_val b, s_ds s1, s_ds s2)) s
s_ds (While b s1) s = (fix ff) s
  where
    ff :: (State -> State) -> (State -> State)
    ff g = cond (b_val b, g.s_ds s1, id)




s :: State
s "x" = 4
s "y" = 0
s "z" = 0
s  _  = 0

testA :: Aexp
testA = Mult (Add (V "x") (V "y")) (Sub (V "z") (N 1))

testB :: Bexp
testB = (Neg (Eq (Add (V "x")(V "y")) (N 4)))

s' :: State
s' = update s 5 "x"

testStm :: Stm
testStm = 
  (Comp
    (Ass "y" (N 1))
    (While
     (Neg (Eq (V "x") (N 1)))
      (Comp
        (Ass "y" (Mult (V "y") (V "x")))
        (Ass "x" (Sub (V "x") (N 1)))
        )
    )
  )


-- Natural Semantics with dynamic scope

data Config = Inter Stm State EnvP | Final State EnvP
type EnvP = Pname -> Stm

ns_stm :: Config -> Config
ns_stm (Inter (Ass x a) s envP) = Final (update s (a_val a s) x) envP
ns_stm (Inter (Skip) s envP) = Final s envP
ns_stm (Inter (Comp ss1 ss2) s envP) = Final s'' envP''
  where
  Final s' envP' = ns_stm (Inter ss1 s envP)
  Final s'' envP'' = ns_stm (Inter ss2 s' envP')
ns_stm (Inter (If b ss1 ss2) s envP)
  | b_val b s = Final s' envP
  | otherwise = Final s'' envP
  where
  Final s' envP' = ns_stm (Inter ss1 s envP) 
  Final s'' envP'' = ns_stm (Inter ss2 s envP) 
ns_stm (Inter (While b ss) s envP)
  | b_val b s = Final s'' envP
  | otherwise = Final s envP
  where
  Final s' envP' = ns_stm (Inter ss s envP)
  Final s'' envP'' = ns_stm (Inter (While b ss) s' envP')
ns_stm (Inter (Block decV decP stm) s envP) = Final (restoreState decV s s') envP'
  where 
  Final s' envP' = ns_stm (Inter stm (updateV decV s) (updateP decP envP))
ns_stm (Inter (Call pname) s envP) =  ns_stm (Inter (envP pname) s envP) 

s_ns :: Stm -> State -> State
s_ns ss s = s'
  where
  Final s' envP = ns_stm (Inter ss s envP)

updateV :: DecV -> State -> State
updateV [] s = s
updateV ((var, aexp):xs) s = updateV xs (update s (a_val aexp s) var)


restoreState :: DecV -> State -> State -> State
restoreState [] sOld s y = s y
restoreState ((var,aexp):xs) sOld s y = restoreState xs sOld (rs var sOld s) y

rs :: Var -> State -> State -> State
rs var sOld s y 
  | var == y  = sOld y
  | otherwise= s y



updateP :: DecP -> EnvP -> EnvP
-- In Full [(Pname,Stm)] -> Pname -> Stm -> Pname -> Stm
-- dp :: DecP
-- envP :: Pname -> Stm
-- envPname :: string 
updateP [] envP pname' = envP pname'
updateP ((pname, stm):xs) envP pname'
  | pname == pname' = stm
  | otherwise = updateP xs envP pname'

s_dynamic :: Stm -> State -> State
s_dynamic ss s = s'
  where
    Final s' envP' = ns_stm (Inter ss s envP)
    envP _ = Skip


-- Natural Semantics with Mixed scope
data MConfig = MInter Stm State MEnvP | MFinal State MEnvP
type MEnvP = Pname -> MType
data MType = MTypeC Stm MEnvP | Empty


updateMP :: DecP -> MEnvP -> MEnvP
updateMP [] envP pname = envP pname
updateMP (x:xs) envP pname' = updateMP xs (uMP x envP) pname'

uMP :: (Pname,Stm) -> MEnvP -> MEnvP
uMP (pname',stm) envP pname
  | pname' == pname = MTypeC stm envP
  | otherwise = envP pname

mNS_stm :: MConfig -> MConfig
mNS_stm (MInter (Ass x a) s envP) = MFinal (update s (a_val a s) x) envP
mNS_stm (MInter (Skip) s envP) = MFinal s envP
mNS_stm (MInter (Comp ss1 ss2) s envP) = MFinal s'' envP''
  where
  MFinal s' envP' = mNS_stm (MInter ss1 s envP)
  MFinal s'' envP'' = mNS_stm (MInter ss2 s' envP')
mNS_stm (MInter (If b ss1 ss2) s envP)
  | b_val b s = MFinal s' envP
  | otherwise = MFinal s'' envP
  where
  MFinal s' envP' = mNS_stm (MInter ss1 s envP) 
  MFinal s'' envP'' = mNS_stm (MInter ss2 s envP) 
mNS_stm (MInter (While b ss) s envP)
  | b_val b s = MFinal s'' envP
  | otherwise = MFinal s envP
  where
  MFinal s' envP' = mNS_stm (MInter ss s envP)
  MFinal s'' envP'' = mNS_stm (MInter (While b ss) s' envP')
mNS_stm (MInter (Block decV decP stm) s envP) = MFinal (restoreState decV s s') envP'
  where 
  MFinal s' envP' = mNS_stm (MInter stm (updateV decV s) (updateMP decP envP))
mNS_stm (MInter (Call pname) s envP) =  MFinal s' envP''
  where 
  MFinal s' envP'' = mNS_stm (MInter (stm) s envP')
  MTypeC stm envP' = envP pname
  Empty = undefined


s_mixed :: Stm -> State -> State
s_mixed ss s = s'
  where
    MFinal s' envP' = mNS_stm (MInter ss s envP)
    envP _ = Empty      -- I think this is causing the problem

testProg = "/*fac call (p.55)*/ begin proc fac is begin var z := x; if x = 1 then skip else ( x := x - 1; call fac; y := z * y ) end; y := 1; call fac end"
testProg2 = "begin var y := 1; (x:= 1; begin var x :=2; y:=x+1 end; x:= y +x) end"


actualScopeProg = Block [("x",N 0)] [("p",Ass "x" (Mult (V "x") (N 2))),("q",Call "p")] (Block [("x",N 5)] [("p",Ass "x" (Add (V "x") (N 1)))] (Comp (Call "q") (Ass "y" (V "x"))))




scopeProg2 = Block [("y",N 1)] [] (Comp (Ass "x" (N 1)) (Comp (Block [("x",N 2)] [] (Ass "y" (Add (V "x") (N 1)))) (Ass "x" (Add (V "y") (V "x")))))

scopeProg3 = Block [] [("fac",Block [("z",V "x")] [] (If (Eq (V "x") (N 1)) Skip (Comp (Ass "x" (Sub (V "x") (N 1))) (Comp (Call "fac") (Ass "y" (Mult (V "z") (V "y")))))))] (Comp (Ass "y" (N 1)) (Call "fac"))
