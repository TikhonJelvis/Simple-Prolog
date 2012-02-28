module Prolog.Parse (program, query) where

import Control.Applicative (liftA2, (<$>), (<*), (*>), (<*>))

import Text.ParserCombinators.Parsec

import Prolog.Interpreter

idChar :: Parser Char
idChar = letter <|> digit <|> char '_'

name :: Parser String
name = liftA2 (:) lower (many idChar) <* spaces

atom :: Parser Term
atom = Atom <$> name

variable :: Parser Term
variable = Var . Name 0 <$> liftA2 (:) upper (many idChar) <* spaces

args :: Parser [Term]
args = char '(' *> term `sepBy` (char ',' <* spaces) <* char ')' <* spaces

predicate :: Parser Predicate
predicate = Predicate <$> name <*> args <* spaces

term :: Parser Term
term = try (Pred <$> predicate) <|> atom <|> variable

rule ::  Parser Rule
rule =  (try (Rule <$> predicate <* string ":-" <* spaces <*> body)
    <|> (`Rule` []) <$> predicate) <* spaces
  where body = predicate `sepBy` (char ',' <* spaces) <* char '.'

rules :: Parser [Rule]
rules = (spaces *> rule) `sepBy` char '\n'

query :: Parser Predicate
query = predicate <* char '?'

program :: Parser ([Rule], Maybe Predicate)
program = liftA2 (,) rules $ Just <$> query <|> return Nothing