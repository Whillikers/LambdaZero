-- FOL prover roughly based on Jan van Eijck's "Tutorial on Theorem Proving"
module FOLProver
    -- TODO: expose interface
    -- (prove)
      where

import Control.Exception (assert)
import Data.Maybe
import Data.Either
import qualified Data.Set as S
import qualified Data.Map.Strict as M

import Control.Monad
import Control.Monad.Supply
import Control.Monad.Trans.Identity

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

-- Positive literals, negative literals, unexpanded formulas
data Branch = Br (S.Set F.Term) (S.Set F.Term) [F.Formula]
    deriving Show
type Tableau = [Branch]

-- Prove a statement of first-order logic
prove :: T.Theorem -> Int -> Bool
prove thm depth =
    let axioms = map F.toSkolemForm (T.hypothesis thm)
        conjecture = F.toSkolemForm $ F.neg $ T.conclusion thm
        initialTab = [Br S.empty S.empty (conjecture : axioms)]
        names = map (\i -> 'v' : show(i)) [1..]
        expanded = runListSupply (depthExpand initialTab depth) names
     in case expanded of
         Right tab -> not . null . tabMGUs $ tab
         _ -> False
  where
    -- Expand all branches out to a certain depth
    depthExpand :: Tableau -> Int -> (Supply String) Tableau
    depthExpand tab 0 = return tab
    depthExpand tab d = do
        let open = filter (not . fullyExpanded) tab
        step <- mapM expand open
        depthExpand (concat step) (d - 1)

    -- Expand the chosen branch until a gamma-rule, returning all sub-branches
    -- None if the branch is fully expanded
    expand :: Branch -> (Supply String) [Branch]
    expand branch@(Br _ _ []) = return [branch] -- Full expansion
    expand (Br pos neg (atom@(P _ _) : unExp)) = -- Positive literal
        let lit = L.literalToTerm atom in
            if S.member lit neg then return []
                                else expand (Br (S.insert lit pos) neg unExp)
    expand (Br  pos neg ((N atom@(P _ _)) : unExp)) = -- Negative literal
        let lit = L.literalToTerm atom in
            if S.member lit pos then return []
                                else expand (Br pos (S.insert lit neg) unExp)
    expand (Br pos neg ((B "&" x y):unExp)) = -- Alpha-rule (conjunction)
        expand (Br pos neg (x:unExp ++ [y]))
    expand (Br pos neg ((B "|" x y):unExp)) = -- Beta-rule (disjunction)
        (liftM2 (++)) (expand (Br pos neg (x:unExp))) (expand (Br pos neg (y:unExp)))
    expand (Br pos neg (quant@(Q "V" var form) : unExp)) = do -- Gamma-rule
        newName <- demand
        let inst = F.subFormula (M.singleton var (F.var newName)) form
        return [Br pos neg (inst : unExp ++ [quant])]

    fullyExpanded :: Branch -> Bool
    fullyExpanded (Br _ _ []) = True
    fullyExpanded _ = False

    -- List of all MGUs that would close the branch
    branchMGUs :: Branch -> [U.Unifier]
    branchMGUs (Br pos neg _) =
        let maybeSet = S.map (uncurry L.termMGU) (S.cartesianProduct pos neg)
         in S.toList . (S.map fromJust) . (S.filter isJust) $ maybeSet

    -- Lazy list of all MGUs that would close the tableau
    tabMGUs :: Tableau -> [U.Unifier]
    tabMGUs [] = [L.idUnifier]
    tabMGUs [branch] = branchMGUs branch
    tabMGUs (b:tab) = concat [tabMGUs (subTab u tab) | u <- branchMGUs b]

    -- Apply substitution to a branch
    subBranch :: U.Unifier -> Branch -> Branch
    subBranch unif (Br pos neg forms) = Br pos' neg' forms'
      where
        pos' = S.map (F.subTerm unif) pos
        neg' = S.map (F.subTerm unif) neg
        forms' = map (F.subFormula unif) forms

    -- Apply substitution to a tableau
    subTab :: U.Unifier -> Tableau -> Tableau
    subTab unif = map (subBranch unif)
