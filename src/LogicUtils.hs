module LogicUtils where

import Data.Maybe
import qualified Data.List as L
import qualified Data.Set as S
import qualified Data.Map.Strict as Map
import Control.Monad.Supply

import qualified Folly.Formula as F
import qualified Folly.Unification as U
import Folly.Formula (Term (..), Formula (..))

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

conjoinList :: [F.Formula] -> F.Formula
conjoinList [] = F.t -- The empty conjunction is true
conjoinList fs = foldr1 F.con fs

disjoinList :: [F.Formula] -> F.Formula
disjoinList [] = F.f -- The empty disjunction is false
disjoinList fs = foldr1 F.dis fs

-- Utilities to help with Folly
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

-- Gets (function names/arities, predicate names/arities) from a formula
getFuncsAndPreds :: F.Formula -> (S.Set (String, Int), S.Set (String, Int))
getFuncsAndPreds T = (S.empty, S.empty)
getFuncsAndPreds F = (S.empty, S.empty)
getFuncsAndPreds (B _ f1 f2) = (S.union funcs1 funcs2, S.union preds1 preds2)
  where (funcs1, preds1) = getFuncsAndPreds f1
        (funcs2, preds2) = getFuncsAndPreds f2
getFuncsAndPreds (N f) = getFuncsAndPreds f
getFuncsAndPreds (P name terms) = (funcs, preds) -- Exclude equality
  where funcs = S.unions (map getTermFuncs terms)
        preds = (if name == "Equals" then S.empty
                                     else S.singleton (name, length terms))
getFuncsAndPreds (Q _ term form) = (S.union funcs (getTermFuncs term), preds)
  where (funcs, preds) = getFuncsAndPreds form

-- Gets function names and arities from a term
getTermFuncs :: F.Term -> (S.Set (String, Int))
getTermFuncs (Constant _) = S.empty
getTermFuncs (Var _) = S.empty
getTermFuncs (Func name terms) =
    S.insert (name, length terms) (S.unions (map getTermFuncs terms))

-- Redundant with foldr, but makes several things easier to think about
quantifyList :: (F.Term -> F.Formula -> F.Formula) -> [F.Term] -> F.Formula -> F.Formula
quantifyList _ [] form = form
quantifyList quant terms form = foldr quant form terms
