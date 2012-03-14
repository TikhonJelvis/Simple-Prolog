module Prolog.Interpreter (simplify, disjoin, subst, contains, resolve,
                           Term(..), Name(..), Rule(..), Predicate(..), MGU)  where

import Control.Applicative ((<$>), (<*>))
import Control.Arrow       (second)
import Data.List           (find, nub)
import Control.Monad
import Data.Maybe

data Term = Atom String
          | Var Name
          | Pred Predicate deriving (Show, Eq)
                                    
data Name = Name Int String deriving (Show, Eq)
                                    
data Rule = Rule Predicate [Predicate] deriving (Show, Eq)
                                            
data Predicate = Predicate Bool String [Term] deriving (Show, Eq)

type MGU = [(Name, Term)] -- This should map variables to values.

merge :: MGU -> MGU -> MGU
merge left right = left ++ (second (subst left) <$> right)

freshen :: Rule -> Rule
freshen (Rule hd body) = Rule (freshenPred hd) $ freshenPred <$> body
  where freshenPred (Predicate a n args) = Predicate a n $ freshenTerm <$> args
        freshenTerm (Var (Name i n)) = Var $ Name (i + 1) n
        freshenTerm (Pred p)         = Pred $ freshenPred p
        freshenTerm term             = term

substPred :: MGU -> Predicate -> Predicate
substPred mgu (Predicate a n b) = Predicate a n $ subst mgu <$> b

subst :: MGU -> Term -> Term
subst mgu var@(Var name) = fromMaybe var $ lookup name mgu
subst mgu (Pred p)       = Pred $ substPred mgu p
subst _ atom             = atom

unify :: Predicate -> Predicate -> Maybe MGU
unify (Predicate _ name1 body1) (Predicate _ name2 body2)
  | name1 /= name2 || length body1 /= length body2 = Nothing
  | otherwise = foldM combine [] $ zip body1 body2
  where combine mgu (left, right) = go mgu (subst mgu left) (subst mgu right)
        go mgu (Var l) r | not (r `contains` Var l) = Just $ (l, r) : mgu
        go mgu l (Var r) | not (l `contains` Var r) = Just $ (r, l) : mgu
        go mgu (Pred l) (Pred r)                  = merge <$> unify l r <*> Just mgu
        go mgu l r                                = if l == r then Just mgu else Nothing 
        
contains :: Term -> Term -> Bool
contains v1@Var{} v2@Var{}          = v1 == v2
contains (Pred (Predicate _ _ p)) n = or $ (`contains` n) <$> p
contains _ _                        = False

resolve :: Predicate -> [Rule] -> [MGU]
resolve goal rules = mapMaybe match (freshen <$> rules) >>= exec
  where match rule@(Rule hd _) = (,) rule <$> unify goal hd
        exec ((Rule _ body), mgu) = join (map . second . subst) <$> foldM append mgu body
        append mgu p@(Predicate True _ _) = merge mgu <$> resolve (substPred mgu p) (freshen <$> rules)
        append mgu p = if null . resolve (substPred mgu p) $ freshen <$> rules then [mgu] else []
        
disjoin :: [Predicate] -> (Predicate, Rule) 
disjoin preds = (goal, Rule goal $ preds)
  where goal = Predicate True "*" . nub $ preds >>= \ (Predicate _ _ t) -> t
        
simplify :: MGU -> MGU
simplify []         = []
simplify ((n,v):rs) = (n, fromMaybe v $ Var <$> replacement) : rest
  where replacement = fst <$> find ((== v) . snd) rs
        rest = simplify $ if isJust replacement then filter ((/= n) . fst) rs else rs
