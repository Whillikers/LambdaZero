module LogicUtils where

import qualified Data.Set as S

import qualified Folly.Formula as F
import Folly.Formula (Term (..), Formula (..))

boundVars :: F.Formula -> S.Set F.Term
boundVars form = S.difference (F.vars form) (F.freeVars form)

isLiteral :: F.Formula -> Bool
isLiteral (P _ _) = True
isLiteral (N form) = isLiteral form
isLiteral _ = False
