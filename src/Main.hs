module Main where

import Data.Functor ((<$>))
import Data.List (intercalate)

import System.Environment

import Text.ParserCombinators.Parsec

import Prolog.Interpreter
import Prolog.Parse

showResult :: Predicate -> [MGU] -> [String]
showResult _ []      = ["No"]
showResult q res = map (showMgu . filter (contains (Pred q) . Var . fst)) res
  where showMgu []  = "Yes"
        showMgu mgu = intercalate " " $ map (\(n,v) -> showName n ++ " = " ++ showVal v) mgu
        showName (Name 0 n) = n
        showName (Name i n) = n ++ "_" ++ show i
        showVal (Atom atom) = atom
        showVal (Var n)     = showName n
        showVal (Pred p)    = show p
        
repl :: String -> (String -> IO ()) -> IO ()
repl prompt action = do putStr prompt
                        inp <- getLine
                        case inp of
                          "quit" -> return ()
                          _      -> action inp >> repl prompt action

main :: IO ()
main = do args <- getArgs
          case args of
            []     -> putStrLn "Please specify a file to run."
            [file] -> run file
            _      -> putStrLn "Please only specify one file!"

run :: FilePath -> IO ()
run file = do source <- readFile file
              let program = parse rules file source
              repl "?-" $ go program . parse query "<interactive>"
                where go _ (Left err)   = putStrLn $ "Error: " ++ show err
                      go prog (Right q) = case resolve q <$> prog of
                        Left err -> putStrLn $ "Error: " ++ show err
                        Right a  -> mapM_ putStrLn $ showResult q a 
