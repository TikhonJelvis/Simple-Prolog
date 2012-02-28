module Main where

import Control.Arrow  (second)
import Control.Monad
import Data.Functor   ((<$>))
import Data.Maybe

data Term = Atom String
          | Var Name
          | Pred Predicate deriving (Show, Eq)
                                    
data Name = Name String Int deriving (Show, Eq)
                                    
data Rule = Rule Predicate [Predicate] deriving (Show, Eq)
                                            
data Predicate = Predicate {pName :: String, pArgs :: [Term]} deriving (Show, Eq)

type MGU = [(Name, Term)] -- This should map variables to values.

merge :: Maybe MGU -> Maybe MGU -> Maybe MGU
merge = liftM2 $ \ left right -> left ++ (second (subst left) <$> right)

freshen :: Rule -> Rule
freshen (Rule hd body) = Rule (freshenPred hd) $ freshenPred <$> body
  where freshenPred (Predicate n args) = Predicate n $ freshenTerm <$> args
        freshenTerm (Var (Name n i))   = Var . Name n $ i + 1
        freshenTerm term               = term

substPred :: MGU -> Predicate -> Predicate
substPred mgu (Predicate n b) = Predicate n $ subst mgu <$> b

subst :: MGU -> Term -> Term
subst mgu var@(Var name)       = fromMaybe var (lookup name mgu)
subst mgu (Pred p@Predicate{}) = Pred $ substPred mgu p
subst _ atom                   = atom
                                 
contains :: Term -> Term -> Bool
contains v1@Var{} v2@Var{}        = v1 == v2
contains (Pred (Predicate _ p)) n = or $ (`contains` n) <$> p
contains _ _                      = False

unify :: Predicate -> Predicate -> Maybe MGU
unify (Predicate name1 body1) (Predicate name2 body2)
  | name1 /= name2 || length body1 /= length body2 = Nothing
  | otherwise = foldM combine [] $ zip body1 body2
  where combine mgu (left, right) = go mgu (subst mgu left) (subst mgu right)
        go mgu (Var l) r | not (r `contains` Var l) = Just $ (l, r) : mgu
        go mgu l (Var r) | not (l `contains` Var r) = Just $ (r, l) : mgu
        go mgu (Pred left) (Pred right)
          | pName left == pName right = unify left right `merge` Just mgu
        go mgu l r = if l == r then Just mgu else Nothing 
        
resolve :: Predicate -> [Rule] -> [MGU]
resolve goal rules = mapMaybe match rules >>= exec
  where match rule@(Rule hd _) = (,) rule <$> unify goal hd
        exec ((Rule _ body), mgu) = foldM (append $ freshen <$> rules) mgu body
        append freshRules mgu p = resolve (substPred mgu p) freshRules
