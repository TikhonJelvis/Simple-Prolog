module Main where

import Control.Arrow  (second)
import Control.Monad
import Data.Functor   ((<$>))
import Data.Maybe

data Term = Atom String
          | Var String
          | Pred Predicate deriving (Show, Eq)
                                    
data Rule = Rule Predicate [Predicate] deriving (Show, Eq)
                                            
data Predicate = Predicate {name :: String, args :: [Term]} deriving (Show, Eq)

type MGU = [(String, Term)] -- This should map variables to values.

merge :: Maybe MGU -> Maybe MGU -> Maybe MGU
merge = liftM2 $ \ left right -> left ++ map (second $ subst left) right

substPred :: MGU -> Predicate -> Predicate
substPred mgu (Predicate n b) = Predicate n $ map (subst mgu) b

subst :: MGU -> Term -> Term
subst mgu (Var name)           = maybe (Var name) id (lookup name mgu)
subst mgu (Pred p@Predicate{}) = Pred $ substPred mgu p
subst mgu atom                 = atom
                                 
contains :: Term -> Term -> Bool
contains (Var n1) (Var n2)        = n1 == n2
contains (Pred (Predicate _ p)) n = or $ map (`contains` n) p
contains _ _                      = False

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
        
resolve :: Predicate -> [Rule] -> [MGU]
resolve goal rules = mapMaybe match rules >>= execute
  where match rule@(Rule head _) = (,) rule <$> unify goal head
        execute ((Rule _ body), mgu) = foldM append mgu body
        append mgu pred = resolve (substPred mgu pred) rules
