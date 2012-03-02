module Main where

import Control.Monad

import Control.Applicative ((<$), (<*), (<$>))
import Data.List (intercalate)

import System.Environment

import Text.ParserCombinators.Parsec

import Prolog.Interpreter
import Prolog.Parse

type Parsed = Either ParseError

showResult :: Predicate -> [MGU] -> [String]
showResult _ []      = ["No"]
showResult q res = showMgu . simplify . filter (contains (Pred q) . Var . fst) . reverse <$> res
  where showMgu []  = "Yes"
        showMgu mgu = intercalate " " $ map showBinding mgu
        showBinding (n,v) = showName n ++ " = " ++ showVal v
        showName (Name 0 n) = n
        showName (Name i n) = n ++ "_" ++ show i
        showVal (Atom atom) = atom
        showVal (Var n)     = showName n
        showVal (Pred p)    = showPred p
        showPred (Predicate _ n b) = n ++ "(" ++ intercalate ", " (showVal <$> b) ++ ")"
        
repl :: String -> (String -> IO ()) -> IO ()
repl prompt action = putStr prompt >> getLine >>= go
  where go "quit" = return ()
        go inp    = action inp >> repl prompt action

main :: IO ()
main = do args <- getArgs
          case args of
            []     -> putStrLn "Please specify a file to run."
            [file] -> run file
            _      -> putStrLn "Please only specify one file!"

run :: FilePath -> IO ()
run file = do source <- readFile file
              let program = parse rules file source
              repl "?-" $ go . extractQuery program
  where go (Left err)        = putStrLn $ "Error: " ++ show err
        go (Right (prog, q)) = printResults q $ resolve q prog

extractQuery :: Parsed [Rule] -> String -> Parsed ([Rule], Predicate)
extractQuery program input = do source  <- program
                                queries <- parse query "<interactive>" input
                                let (q,r) = disjoin queries
                                return (r:source, q)

printResults :: Predicate -> [MGU] -> IO ()
printResults q a = foldM_ (\ () b -> () <$ putStr b <* getLine) () $ showResult q a 
