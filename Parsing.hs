{-# LANGUAGE InstanceSigs #-}
module Parsing where

import Data.Char
import Prelude hiding ((>>=), return, (>>))
import Control.Monad
import Data.Functor

newtype Parser a = Parser (String -> [(a, String)])

instance Functor Parser where
  fmap :: (a -> b) -> Parser a -> Parser b
  fmap f (Parser p1) = Parser p2
    where
      p2 s = convert (p1 s)
      convert = map (\(s, val) -> (f s, val))

instance Applicative Parser where
  pure x = Parser (\s -> [(x, s)])
  (Parser pf) <*> (Parser px) = Parser (\s -> [(f x, sx) | (f, sf) <- pf s, (x, sx) <- px sf])

instance Monad Parser where
      return :: a -> Parser a
      return v = Parser (\inp -> [(v, inp)])
      p >>= f = Parser(\inp -> case parse p inp of
        [] -> []
        [(v, out)] -> parse (f v) out)
      (>>) :: Parser a -> Parser b -> Parser b
      p >> q = p >>= \_ -> q


failure :: Parser a
failure = Parser(\_ -> [])

parse :: Parser a -> String -> [(a, String)]
parse (Parser p) = p

item :: Parser Char
item = Parser(\inp -> case inp of
  [] -> []
  (x:xs) -> [(x, xs)])

(+++) :: Parser a -> Parser a -> Parser a
p +++ q = Parser(\inp -> case parse p inp of
  [] -> parse q inp
  [(v, out)] -> [(v, out)])

many :: Parser a -> Parser [a]
many p = many1 p +++ return []

many1 :: Parser a -> Parser [a]
many1 p = do
            v  <- p
            vs <- many p
            return (v:vs) 

sat :: (Char -> Bool) -> Parser Char
sat p = do
          x <- item
          if p x 
              then 
                  return x 
              else 
                  failure

digit :: Parser Char
digit = sat isDigit

char :: Char -> Parser Char
char c = sat (==c)

alphanum :: Parser Char
alphanum = sat isAlphaNum

nat :: Parser Int
nat = do
        num <- (many digit)
        return (read num)

lower :: Parser Char
lower = sat isLower

ident :: Parser String
ident = do
          x  <- lower 
          xs <- many alphanum 
          return (x:xs)

string :: String -> Parser String
string [] = return []
string (x:xs) = do
                  c  <- char x
                  cs <- string xs
                  return (c:cs)

space :: Parser ()
space = do
          many (sat isSpace)
          return ()

token :: Parser a -> Parser a
token p = do
            space 
            x <- p
            space
            return x

natural :: Parser Int
natural = token nat

symbol :: String -> Parser String
symbol xs = token (string xs)

expr :: Parser Int
expr = do
          t<-term
          (do
              symbol "+"
              e <- expr
              return (t + e)) +++ return t


term :: Parser Int
term = do
         f<-factor
         (do
            symbol "*"
            t <- term
            return (f * t)) +++ return f


factor :: Parser Int
factor =  do
            (do
                symbol "("
                e <- expr
                symbol ")"
                return e) +++ natural

