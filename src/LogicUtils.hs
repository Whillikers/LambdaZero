module LogicUtils where

import Data.Maybe
import qualified Data.List as L
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
idUnifier = Map.empty :: U.Unifier

composeUnif :: U.Unifier -> U.Unifier -> U.Unifier
composeUnif u1 u2 =
    let l1 = Map.toList u1
        l2 = Map.toList u2
        u2' = [(k, U.applyUnifier u1 v) | (k, v) <- l2]
        (domain, _) = unzip u2'
        u1' = filter (\(k, v) -> notElem k domain) l1
        simplified = (filter (\(k, v) -> k /= v) u2') ++ u1'
     in Map.fromList simplified

termMGU :: F.Term -> F.Term -> Maybe U.Unifier
termMGU (Constant x) (Constant y) = if x == y then Just idUnifier else Nothing
termMGU x@(Constant _) y@(Var _) = Just (Map.singleton y x)
termMGU (Constant x) (Func y []) = if x == y then Just idUnifier else Nothing
termMGU (Constant x) (Func _ _) = Nothing
termMGU x@(Var name1) y@(Var name2)
  | name1 == name2 = Just idUnifier
  | otherwise = Just (Map.fromList [(x, y)])
termMGU x@(Var _) y@(Func _ funcTerms)
  | elem x funcTerms = Nothing
  | otherwise = Just (Map.singleton x y)
termMGU x@(Func name1 terms1) y@(Func name2 terms2)
  | name1 /= name2 = Nothing
  | otherwise = listMGU terms1 terms2
termMGU x y = termMGU y x

listMGU :: [F.Term] -> [F.Term] -> Maybe U.Unifier
listMGU [] [] = Just idUnifier
listMGU [] (_:_) = Nothing
listMGU (_:_) [] = Nothing
listMGU (x:xs) (y:ys) = do
    u1 <- termMGU x y
    u2 <- listMGU (map (U.applyUnifier u1) xs) (map (U.applyUnifier u1) ys)
    Just (composeUnif u2 u1)
