module Prover
    -- TODO: expose interface
    -- (proveProp)
      where

import qualified Data.Set as S

import Folly.Formula as F
import Folly.Theorem as T
import Folly.Unification as U

-- Test expressions for propositional logic
a = F.imp (F.pr "P" [F.constant "x"]) (F.pr "P" [F.constant "y"])
b = F.pr "P" [F.constant "x"]
c = F.pr "P" [F.constant "y"]
thmProp = T.theorem [a, b] c

-- Test expressions for first-order logic
d = F.fa (F.var "x") $ F.pr "P" [F.var "x"]
e = F.te (F.var "y") $ F.pr "P" [F.var "y"]
thmFOL = T.theorem [d] e

-- Propositional prover: no quantifiers
proveProp :: T.Theorem -> Bool
proveProp thm =
    let axiomClauses = concatMap F.toClausalForm $ T.hypothesis thm
        conjectureClauses = F.toClausalForm $ F.neg $ T.conclusion thm
        clauses = conjectureClauses ++ axiomClauses
     in refute clauses S.empty
  where
    refute :: [[F.Formula]] -> S.Set F.Formula -> Bool
    refute [] lits = False
    refute (clause : rest) lits = and $ map (close rest lits) clause

    close :: [[F.Formula]] -> S.Set F.Formula -> F.Formula -> Bool
    close clauses lits lit = contradiction lit lits ||
        refute clauses (S.insert lit lits)

    contradiction :: F.Formula -> (S.Set F.Formula) -> Bool
    contradiction lit lits = S.member (F.toPNF $ F.neg lit) lits

-- FOL prover:  with quantifiers
-- TODO
