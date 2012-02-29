module Prolog.Parse (rules, query) where

import Control.Applicative (liftA2, (<$>), (<*), (*>), (<*>))

import Text.ParserCombinators.Parsec

import Prolog.Interpreter

idChar :: Parser Char
idChar = letter <|> digit <|> char '_'

name :: Parser String
name = liftA2 (:) lower (many idChar) <* spaces

atom :: Parser Term
atom = Atom <$> name <?> "atom"

variable :: Parser Term
variable = Var . Name 0 <$> liftA2 (:) upper (many idChar) <* spaces <?> "variable"

args :: Parser [Term]
args = char '(' *> term `sepBy` (char ',' <* spaces) <* char ')' <* spaces

predicate :: Parser Predicate
predicate = Predicate <$> name <*> args <?> "predicate"

term :: Parser Term
term = try (Pred <$> predicate) <|> atom <|> variable <?> "term"

rule ::  Parser Rule
rule =  (try (Rule <$> predicate <* string ":-" <* spaces <*> body)
    <|> (`Rule` []) <$> predicate <?> "rule") <* char '.' <* spaces
  where body = predicate `sepBy` (char ',' <* spaces) 

rules :: Parser [Rule]
rules = spaces *> many1 rule

query :: Parser Predicate
query = predicate <* char '?' <?> "query"
