module Prolog.Parse (rules, query) where

import Control.Applicative (liftA2, (<$>), (<*), (*>), (<*>), (<$))

import Text.ParserCombinators.Parsec

import Prolog.Interpreter

idChar :: Parser Char
idChar = letter <|> digit <|> char '_'

name :: Parser String
name = liftA2 (:) lower (many idChar) <* spaces

atom :: Parser Term
atom = Atom <$> (name <|> many1 digit) <?> "atom"

variable :: Parser Term
variable = Var . Name 0 <$> liftA2 (:) upper (many idChar) <* spaces <?> "variable"

args :: Parser [Term]
args = char '(' *> term `sepBy` (char ',' <* spaces) <* char ')' <* spaces

predicate :: Parser Predicate
predicate = negated <|> normal True
  where normal active  =  try (list active)
                      <|> Predicate active <$> name <*> args <?> "predicate"
        negated = (string "~" <|> string "\\+") *> spaces *> (normal False)
        list active = do char '[' *> spaces
                         cars <- term `sepBy1` (char ',' *> spaces)
                         cdr <- rest <* spaces
                         let end = Predicate active "cons" [last cars, cdr]
                         return . foldr (cons active) end $ init cars
        rest =  char '|' *> spaces *> term <* char ']'
            <|> Atom "nil" <$ char ']'
        cons active term rest = Predicate active "cons" [term, Pred rest]

term :: Parser Term
term = try (Pred <$> predicate) <|> atom <|> variable <?> "term"


rule ::  Parser [Rule]
rule = do hd     <- predicate
          bodies <- end <|> string ":-" *> body `sepBy` (char ';' *> spaces)
          return $ Rule hd <$> bodies
  where end  = [[]] <$ char '.' <* spaces
        body = predicate `sepBy` (char ',' <* spaces) 

rules :: Parser [Rule]
rules = spaces *> (concat <$> many1 rule)

query :: Parser [Predicate]
query = spaces *> predicate `sepBy` (char ',' *> spaces) <* char '.' <?> "query"
