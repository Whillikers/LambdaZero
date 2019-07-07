-- Utilities to convert statements of FOL with equality into complete Folly

module FOLEquality where

import qualified Data.Set as S

import qualified Folly.Formula as F

import LogicUtils

-- Add the necessary equality axioms to a set of formulas
addEqualityAxioms :: [F.Formula] -> [F.Formula]
addEqualityAxioms forms = forms ++ basicAxioms ++ functionAxioms ++ predAxioms
  where
    (funcs, preds) = unzip $ map getFuncsAndPreds forms
    functionAxioms = S.toList $ S.map (uncurry functionSub) (S.unions funcs)
    predAxioms = S.toList $ S.map (uncurry predicateSub) (S.unions preds)

-- Predicate-based equality
equals :: F.Term -> F.Term -> F.Formula
equals t1 t2 = F.pr "Equals" [t1, t2]

-- Basic axioms of equality
basicAxioms = [reflexivity, symmetry, transitivity]

reflexivity = F.fa v (equals v v)
  where v = F.var "A"
symmetry = F.fa v1 (F.fa v2 (F.imp (equals v1 v2) (equals v2 v1)))
  where v1 = F.var "A"; v2 = F.var "B"
transitivity = F.fa v1 (F.fa v2 (F.fa v3
    (F.imp
        (F.con (equals v1 v2) (equals v2 v3))
        (equals v1 v3))))
  where v1 = F.var "A"; v2 = F.var "B"; v3 = F.var "C"

-- Function substitution axiom
functionSub :: String -> Int -> F.Formula
functionSub name arity =
    quantifyList F.fa (vars1 ++ vars2) (F.imp varEquality funcEquality)
      where vars1 = [F.var $ "A" ++ (show i) | i <- [1..arity]]
            vars2 = [F.var $ "B" ++ (show i) | i <- [1..arity]]
            func1 = F.func name vars1
            func2 = F.func name vars2
            varEquality = conjoinList $ map (uncurry equals) (zip vars1 vars2)
            funcEquality = equals func1 func2

-- Leibniz's Law
predicateSub :: String -> Int -> F.Formula
predicateSub name arity =
    quantifyList F.fa (vars1 ++ vars2) (F.imp (F.con varEquality pred1) pred2)
      where vars1 = [F.var $ "A" ++ (show i) | i <- [1..arity]]
            vars2 = [F.var $ "B" ++ (show i) | i <- [1..arity]]
            pred1 = F.pr name vars1
            pred2 = F.pr name vars2
            varEquality = conjoinList $ map (uncurry equals) (zip vars1 vars2)
