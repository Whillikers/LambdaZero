module PropositionalProver
    -- TODO: expose interface
    -- (prove)
      where

import Control.Exception (assert)
import qualified Data.Set as S

import Folly.Formula as F
import Folly.Theorem as T
import Folly.Unification as U

-- Test expressions for propositional logic
-- TODO: remove
a = F.imp (F.pr "P" [F.constant "x"]) (F.pr "P" [F.constant "y"])
b = F.pr "P" [F.constant "x"]
c = F.pr "P" [F.constant "y"]
thmProp = T.theorem [a, b] c

-- Prove a statement of propositional logic
prove :: T.Theorem -> Bool
prove thm =
    let axioms = T.hypothesis thm
        conjecture = F.neg $ T.conclusion thm
        axiomClauses = concatMap F.toClausalForm $ axioms
        conjectureClauses = F.toClausalForm $ conjecture
        clauses = conjectureClauses ++ axiomClauses
     in assert
         (and $ map isPropositional (conjecture : axioms))
         (refute clauses S.empty)
  where
    refute :: [[F.Formula]] -> S.Set F.Formula -> Bool
    refute [] lits = False
    refute (clause : rest) lits = and $ map (close rest lits) clause

    close :: [[F.Formula]] -> S.Set F.Formula -> F.Formula -> Bool
    close clauses lits lit = contradiction lit lits ||
        refute clauses (S.insert lit lits)

    contradiction :: F.Formula -> (S.Set F.Formula) -> Bool
    contradiction lit lits = S.member (F.toPNF $ F.neg lit) lits

    isPropositional :: F.Formula -> Bool
    isPropositional f = F.vars f == S.empty
