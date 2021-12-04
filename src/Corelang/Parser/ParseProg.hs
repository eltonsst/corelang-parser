{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Corelang.Parser.ParseProg where

import           Control.Applicative
import           Corelang.Parser.Parse (Parser, alphanum, char, lower, natural,
                                        space, symbol)

type Name = String -- function name
type Program a = [ScDef a] -- program is a list of supercombinators
type ScDef a = (Name, [a], Expr a) -- a supercombinator consists of a name, list of parameters and an expression (the body)
type Def a = (a, Expr a)
type Alter a = (Int, [a], Expr a)
type CoreProgram = Program Name
type CoreScDef = ScDef Name
data IsRec = Recursive | NonRecursive deriving (Show, Eq)

data Expr a
  = EVar Name -- Variables
  | ENum Int -- Numbers
  | EConstr Int Int -- Constructor tag arity
  | EAp (Expr a) (Expr a) -- Applications
  | ELet IsRec [Def a] (Expr a) -- Let (rec) expressions
  | ECase (Expr a) [Alter a] -- Case expression
  | ELam [a] (Expr a) -- Lambda abstractions
  deriving (Show, Eq)

parseProg :: Parser (Program Name)
parseProg = do
  function <- parseScDef -- parse first statement/function
  do
    symbol ";" -- parse ";" at the end of the statement
    functions <- parseProg -- rest of the program
    return (function : functions) -- return the list
   <|> return [function] -- or just the first fun

parseScDef :: Parser (ScDef Name)
parseScDef = do
  name   <- parseVar -- parse name of the function
  params <- many parseVar -- parse list of parameters
  symbol "=" -- parse the "=" symbol
  body <- parseExpr -- parse the body of the function
  return (name, params, body) -- ("f", "x", ???)

parseExpr :: Parser (Expr Name)
parseExpr =
  do
    -- try parse let-in expression: e.g. f x = let y = x * x in y + y
      symbol "let"
      defs <- parseDefs
      symbol "in"
      ELet NonRecursive defs <$> parseExpr
    <|> do
      -- try parse letrec-in expression
          symbol "letrec"
          defs <- parseDefs
          symbol "in"
          ELet Recursive defs <$> parseExpr
    <|> do
      -- try parse case-of expression
          symbol "case"
          expr <- parseExpr
          symbol "of"
          ECase expr <$> parseAlts
    <|> do
      -- try parse lambda expression
          symbol "\\"
          vars <- some parseVar
          symbol "."
          ELam vars <$> parseExpr
    <|> parseExpr1 -- try parse simple expr like "x * x"

parseExpr1 :: Parser (Expr Name) -- parse an expr1 based on the hierarchy, logic is the same for all parseExprs
parseExpr1 =
  do
   expr2 <- parseExpr2 -- try parse as expr2, if fails it is an expr1
   do
    or <- parseOr -- parsing or
    EAp (EAp or expr2) <$> parseExpr1 -- return appliction of OR on expr2 and the result of parsing expr1
    <|> return expr2 -- if fails it's not an expr1 so return expr2

parseExpr2 :: Parser (Expr Name)
parseExpr2 = do
  expr3 <- parseExpr3 -- try parse as expr3
  do
    and <- parseAnd
    EAp (EAp and expr3) <$> parseExpr2
   <|> return expr3

parseExpr3 :: Parser (Expr Name)
parseExpr3 = do
  expr4 <- parseExpr4 -- try parse as expr4
  do
    op <- parseRelop
    EAp (EAp op expr4) <$> parseExpr4
   <|> return expr4

parseExpr4 :: Parser (Expr Name)
parseExpr4 = do
  expr5 <- parseExpr5 -- try parse as expr5
  do
      add <- parseAdd
      EAp (EAp add expr5) <$> parseExpr4
    <|> do
          sub <- parseSub
          EAp (EAp sub expr5) <$> parseExpr5
    <|> return expr5

parseExpr5 :: Parser (Expr Name)
parseExpr5 = do
  expr6 <- parseExpr6 -- try parse as expr6
  do
      mul <- parseMul
      EAp (EAp mul expr6) <$> parseExpr5
    <|> do
          div <- parseDiv
          EAp (EAp div expr6) <$> parseExpr6
    <|> return expr6

parseExpr6 :: Parser (Expr Name)
parseExpr6 = do
  first <- parseAExpr -- parse AExpr
  do
    rest <- some parseAExpr
    return (foldl EAp first rest)
   <|> return first

parseAExpr :: Parser (Expr Name)
parseAExpr =
  EVar
    <$> parseVar
    <|> ENum
    <$> natural
    <|> do
          symbol "Pack{"
          tag <- natural
          symbol ","
          arity <- natural
          symbol "}"
          return (EConstr tag arity)
    <|> do
          symbol "("
          expr <- parseExpr
          symbol ")"
          return expr

parseDefs :: Parser [Def Name]
parseDefs = do
  def <- parseDef
  do
    symbol ";"
    defs <- parseDefs
    return (def : defs)
   <|> return [def]

-- parse definition of  "variable = expression" e.g. y = x * x
parseDef :: Parser (Def Name)
parseDef = do
  variable <- parseVar -- parse 'y'
  symbol "=" -- parse '='
  expression <- parseExpr -- parse 'x * x'
  return (variable, expression) -- return ("y", ???)

parseAlts :: Parser [Alter Name]
parseAlts = do
  alt <- parseAlt
  do
    symbol ";"
    alts <- parseAlts
    return (alt : alts)
   <|> return [alt]

parseAlt :: Parser (Alter Name) -- e.g. "case of <1> a b -> expr"
parseAlt = do
  symbol "<"
  number <- natural
  symbol ">"
  variables <- many parseVar
  symbol "->"
  expression <- parseExpr
  return (number, variables, expression)

parseVar :: Parser Name
parseVar = do
  space -- parse spaces
  first <- lower -- parse first char that must be lowercase
  rest  <- many (alphanum <|> char '_') -- parse rest of the string
  space
  if isKeyword (first : rest) then empty else return (first : rest) -- check if the name is a keyword and return

parseOr :: Parser (Expr a)
parseOr = EVar <$> symbol "|"

parseAnd :: Parser (Expr a)
parseAnd = EVar <$> symbol "&"

parseAdd :: Parser (Expr a)
parseAdd = EVar <$> symbol "+"

parseSub :: Parser (Expr a)
parseSub = EVar <$> symbol "-"

parseMul :: Parser (Expr a)
parseMul = EVar <$> symbol "*"

parseDiv :: Parser (Expr a)
parseDiv = EVar <$> symbol "/"

parseRelop :: Parser (Expr Name)
parseRelop =
  EVar
    <$> (   symbol "=="
        <|> symbol "~="
        <|> symbol ">"
        <|> symbol ">="
        <|> symbol "<"
        <|> symbol "<="
        )

keywords :: [String]
keywords = ["let", "letrec", "case", "Pack", "of", "in"]

isKeyword :: String -> Bool
isKeyword s = s `elem` keywords
