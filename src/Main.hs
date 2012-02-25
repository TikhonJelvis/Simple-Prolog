module Main where

import Control.Arrow (second)
import Control.Monad

import Data.Functor ((<$>))

data Term = Atom String
          | Var String
          | Pred Predicate deriving (Show, Eq)
 
data Rule = Simple Term
          | Complex Term [Term] deriving (Show, Eq)
                                            
data Predicate = Predicate {name :: String, body :: [Term]} deriving (Show, Eq)

data Result = Yes | No | Binding String Term deriving (Show, Eq)

type MGU = [(String, Term)] -- This should map variables to values.

merge :: Maybe MGU -> Maybe MGU -> Maybe MGU
merge = liftM2 $ \ left right -> left ++ map (second $ subst left) right

subst :: MGU -> Term -> Term
subst mgu (Var name)             = maybe (Var name) id (lookup name mgu)
subst mgu (Pred (Predicate n b)) = Pred . Predicate n $ map (subst mgu) b
subst mgu atom                   = atom
                                 
contains :: Term -> Term -> Bool
contains (Var n1) (Var n2)               = n1 == n2
contains (Pred (Predicate _ p)) n@Var{}  = or $ map (`contains` n) p
contains _ _                             = False

unify :: Predicate -> Predicate -> Maybe MGU
unify (Predicate name1 body1) (Predicate name2 body2)
  | name1 /= name2 || length body1 /= length body2 = Nothing
  | otherwise = foldM combine [] $ zip body1 body2
  where combine mgu (left, right) = go mgu (subst mgu left) (subst mgu right)
        go mgu (Var l) r | not (r `contains` Var l) = Just $ (l, r) : mgu
        go mgu l (Var r) | not (l `contains` Var r) = Just $ (r, l) : mgu
        go mgu (Pred left) (Pred right)
          | name left == name right = unify left right `merge` Just mgu
        go mgu l r = if l == r then Just mgu else Nothing 
        
resolve :: Term -> [Term] -> Result
resolve = undefined

main :: IO ()
main = putStr "Not ready yet."