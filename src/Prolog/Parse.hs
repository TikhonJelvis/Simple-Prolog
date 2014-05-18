module Prolog.Parse (rules, query) where

import           Control.Applicative           (liftA2, (*>), (<$), (<$>), (<*),
                                                (<*>))
import           Control.Monad                 (void)

import           Text.ParserCombinators.Parsec

import           Prolog.Interpreter

comment :: Parser ()
comment = () <$ (char '%' *> many (noneOf "\n") *> char '\n') <?> "comment"

whitespace :: Parser ()
whitespace = skipMany (void space <|> comment) <?> "whitespace"

idChar :: Parser Char
idChar = letter <|> digit <|> char '_'

name :: Parser String
name = liftA2 (:) lower (many idChar) <* whitespace

atom :: Parser Term
atom = Atom <$> (name <|> many1 digit) <?> "atom"

variable :: Parser Term
variable = Var . Name 0 <$> liftA2 (:) upper (many idChar) <* whitespace <?> "variable"

args :: Parser [Term]
args = char '(' *> term `sepBy` (char ',' <* whitespace) <* char ')' <* whitespace

predicate :: Parser Predicate
predicate = negated <|> normal True
  where normal active  =  try (list active)
                      <|> Predicate active <$> name <*> args <?> "predicate"
        negated = (string "~" <|> string "\\+") *> whitespace *> (normal False)
        list active = do char '[' *> whitespace
                         cars <- term `sepBy1` (char ',' *> whitespace)
                         cdr <- rest <* whitespace
                         let end = Predicate active "cons" [last cars, cdr]
                         return . foldr (cons active) end $ init cars
        rest =  whitespace *> char '|' *> whitespace *> term <* char ']'
            <|> Atom "nil" <$ char ']'
        cons active term rest = Predicate active "cons" [term, Pred rest]

term :: Parser Term
term = try (Pred <$> predicate) <|> atom <|> variable <?> "term"

rule ::  Parser [Rule]
rule = do hd     <- predicate
          bodies <- end <|> string ":-" *> whitespace *> (body `sepBy` (char ';' *> whitespace)) <* end
          return $ Rule hd <$> bodies
  where end  = [[]] <$ char '.' <* whitespace
        body = predicate `sepBy` (char ',' <* whitespace)

rules :: Parser [Rule]
rules = whitespace *> (concat <$> many1 rule)

query :: Parser [Predicate]
query = whitespace *> predicate `sepBy` (char ',' *> whitespace) <* char '.' <?> "query"
