module LogicUtils where

import qualified Data.Set as S
import qualified Data.Map.Strict as Map

import qualified Folly.Formula as F
import qualified Folly.Unification as U
import Folly.Formula (Term (..), Formula (..))

boundVars :: F.Formula -> S.Set F.Term
boundVars form = S.difference (F.vars form) (F.freeVars form)

isLiteral :: F.Formula -> Bool
isLiteral (P _ _) = True
isLiteral (N form) = isLiteral form
isLiteral _ = False

-- NOTE: negations are not distinguished
literalToTerm :: F.Formula -> F.Term
literalToTerm (P name vars) = Func name vars
literalToTerm (N (P name vars)) = Func name vars
literalToTerm _ = error "formula must be a literal"

-- Unification
idUnifier :: U.Unifier
idUnifier = Map.empty

-- composeUnifiers :: U.Unifier -> U.Unifier -> U.Unifier
-- TODO

termMGUs :: F.Term -> F.Term -> [U.Unifier]
termMGUs (Constant x) (Constant y) = if x == y then [idUnifier] else []
termMGUs x@(Constant _) y@(Var _) = [Map.singleton y x]
termMGUs (Constant x) (Func y []) = if x == y then [idUnifier] else []
termMGUs (Constant x) (Func _ _) = []
termMGUs x@(Var name1) y@(Var name2)
  | name1 == name2 = [idUnifier]
  | otherwise = [Map.fromList [(x, y), (y, x)]] -- TODO: do we need both?
termMGUs x@(Var _) y@(Func _ funcTerms)
  | elem x funcTerms = []
  | otherwise = [Map.singleton x y]
termMGUs x@(Func name1 terms1) y@(Func name2 terms2)
  | name1 /= name2 = []
  | otherwise = listMGUs terms1 terms2
termMGUs x y = termMGUs y x

listMGUs :: [F.Term] -> [F.Term] -> [U.Unifier]
listMGUs [] [] = [idUnifier]
listMGUs [] (_:_) = []
listMGUs (_:_) [] = []
-- TODO: finish
