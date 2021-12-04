{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Corelang.Parser.Parse where

import           Control.Applicative
import           Control.Monad                  ( )
import           Data.Char                      ( isAlpha
                                                , isAlphaNum
                                                , isDigit
                                                , isLower
                                                , isSpace
                                                , isUpper
                                                )

newtype Parser a = P {parse :: String -> [(a, String)]}

item :: Parser Char
item = P
  (\case
    []       -> []
    (c : cs) -> [(c, cs)]
  )

instance Functor Parser where
  fmap f p = P
    (\s -> case parse p s of
      []        -> []
      [(v, s')] -> [(f v, s')]
    )

instance Applicative Parser where
  pure v = P (\s -> [(v, s)])

  pf <*> pa = P
    (\s -> case parse pf s of
      []        -> []
      [(f, s')] -> case parse pa s' of
        []         -> []
        [(a, s'')] -> [(f a, s'')]
    )

instance Monad Parser where
  pa >>= f = P
    (\s -> case parse pa s of
      []        -> []
      [(v, s')] -> parse (f v) s'
    )

instance Alternative Parser where
  empty = P (const [])

  p1 <|> p2 = P
    (\s -> case parse p1 s of
      []        -> parse p2 s
      [(v, s')] -> [(v, s')]
    )

sat :: (Char -> Bool) -> Parser Char
sat p = do
  v <- item
  if p v then return v else empty

digit :: Parser Char
digit = sat isDigit

lower :: Parser Char
lower = sat isLower

upper :: Parser Char
upper = sat isUpper

letter :: Parser Char
letter = sat isAlpha

alphanum :: Parser Char
alphanum = sat isAlphaNum

varch :: Parser Char
varch = alphanum <|> char '_'

char :: Char -> Parser Char
char c = sat (== c)

string :: String -> Parser String
string []       = return []
string (c : cs) = do
  char c
  string cs
  return (c : cs)

nat :: Parser Int
nat = do
  xs <- some digit
  return (read xs)

ident :: Parser String
ident = do
  x  <- letter
  xs <- many varch
  return (x : xs)

space :: Parser ()
space = do
  many (sat isSpace)
  return ()

int :: Parser Int
int =
  (do
      char '-'
      space
      n <- nat
      return (-n)
    )
    <|> nat

token :: Parser a -> Parser a
token p = do
  space
  v <- p
  space
  return v

natural :: Parser Int
natural = token nat

integer :: Parser Int
integer = token int

symbol :: String -> Parser String
symbol s = token (string s)

identifier :: Parser String
identifier = token ident

character :: Char -> Parser Char
character c = token (char c)
