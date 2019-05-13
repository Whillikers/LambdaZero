module FOLProver
    -- TODO: expose interface
    -- (prove)
      where

import Control.Exception (assert)
import qualified Data.Set as S
import qualified Data.Map as M
import Data.Maybe (isJust)

import qualified Folly.Formula as F
import qualified Folly.Theorem as T
import qualified Folly.Unification as U
import Folly.Formula (Term (..), Formula (..))

import qualified LogicUtils as L

-- Test expressions for first-order logic
-- TODO: remove
d = F.dis (F.neg $ F.pr "P" [F.constant "a"])  (F.pr "Q" [F.constant "b"])
e = F.fa (F.var "x") (F.pr "P" [F.var "x"])
g = F.pr "P" [F.constant "b"]
thmFOL = T.theorem [d, e] g

-- Prove a statement of first-order logic
prove :: T.Theorem -> Bool
prove thm =
    let axioms = map F.toSkolemForm (T.hypothesis thm)
        conjecture = F.toSkolemForm $ F.neg $ T.conclusion thm
     in refute conjecture axioms [] (F.freeVars conjecture) 100
  where
    refute :: F.Formula -> [F.Formula] -> [F.Formula] ->
        S.Set F.Term -> Int -> Bool

    -- Conjunction: alpha-rule
    refute (B "&" f1 f2) unexpanded lits frees gammaLimit =
        refute f1 (f2 : unexpanded) lits frees gammaLimit

    -- Disjunction: beta-rule
    refute (B "|" f1 f2) unexpanded lits frees gammaLimit =
        refute f1 (unexpanded) lits frees gammaLimit &&
        refute f2 (unexpanded) lits frees gammaLimit

    -- Universal quantification: gamma-rule
    refute (Q "V" _ _) _ _ _ 0 = False -- Base case: max gamma depth
    refute (Q "V" (variable) f) unexpanded lits frees gammaLimit =
        -- TODO: better way of getting unique names; avoid collisions
        let var_name = F.varName variable
            new_name = var_name ++ "'"
            new_var = F.var new_name
            term_map = M.singleton variable new_var
            new_quant = Q "V" new_var (F.subFormula term_map f)
         in refute f (unexpanded ++ [new_quant]) lits
             (S.insert variable frees) (gammaLimit - 1)

    -- Literal: try branch closure, reduction, and extension
    refute lit unexpanded lits frees gammaLimit =
        (contradiction lit lits) -- Immediate contradiction
      || reduce lit unexpanded lits frees gammaLimit -- Reduction
      || extend lit unexpanded lits frees gammaLimit -- Extension

    contradiction :: F.Formula -> [F.Formula] -> Bool
    contradiction lit lits = elem (F.toPNF $ F.neg lit) lits

    -- Reduction step
    -- TODO: tableau-wide unification
    reduce :: F.Formula -> [F.Formula] -> [F.Formula] ->
        S.Set F.Term -> Int -> Bool
    reduce _ _ [] _ _ = False -- No clauses to contradict
    reduce lit _ (firstLit : rest) _ _ =
        if canUnify (F.toPNF $ F.neg lit) firstLit
           then True -- Can unify to form a contradiction
           else reduce lit [] rest S.empty 0
        where
          -- Is it possible to unify f1 with f2?
          -- TODO: fix bugs with this
          canUnify :: F.Formula -> F.Formula -> Bool
          canUnify (P n1 t1) (N (P n2 t2))
              | n1 == n2 = isJust $ U.mostGeneralUnifier (zip t1 t2)
              | otherwise = False
          canUnify (N (P n1 t1)) (P n2 t2)
              | n1 == n2 = isJust $ U.mostGeneralUnifier (zip t1 t2)
              | otherwise = False
          canUnify _ _ = False

    -- Extension step
    extend :: F.Formula -> [F.Formula] -> [F.Formula] ->
        S.Set F.Term -> Int -> Bool
    extend lit (f : unexpanded) lits frees gammaLimit =
        refute f unexpanded (lit : lits) frees gammaLimit
