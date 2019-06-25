module PropositionalProver (prove) where

import Control.Exception (assert)
import qualified Data.Set as S

import qualified Folly.Formula as F
import qualified Folly.Theorem as T

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
    refute [] _ = False
    refute (clause : rest) lits = and $ map (close rest lits) clause

    close :: [[F.Formula]] -> S.Set F.Formula -> F.Formula -> Bool
    close clauses lits lit = contradiction lit lits ||
        refute clauses (S.insert lit lits)

    contradiction :: F.Formula -> (S.Set F.Formula) -> Bool
    contradiction lit lits = S.member (F.toPNF $ F.neg lit) lits

    isPropositional :: F.Formula -> Bool
    isPropositional f = F.vars f == S.empty
