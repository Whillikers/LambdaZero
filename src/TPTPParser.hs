-- Parses the subset of the TPTP language needed for FOL statements
-- See: http://www.tptp.org/TPTP/SyntaxBNF.html

-- TODO: add Mizar standard axioms

module TPTPParser where

import Data.Either
import Data.Maybe (catMaybes)
import Data.List.NonEmpty (toList)
import Data.List (intercalate, isPrefixOf)
import Data.Text (pack)
import System.FilePath

import qualified Data.TPTP as T
import Data.TPTP.Parse.Text (parseUnitOnly)
import Data.TPTP.Pretty (pretty)

import qualified Folly.Formula as F
import Folly.Theorem (theorem)
import Folly.Formula (Term (..), Formula (..))

import FOLEquality

inputFile = "./data/mizar_statements.txt"

-- Test code
testFile = "./test_problem.txt"
testTheorem = do
    namedFormulas <- fileToFolly testFile
    let formulas = map formulaForm namedFormulas
        axioms = init formulas
        conjecture = last formulas
    return (theorem (addEqualityAxioms axioms) conjecture)

-- Type of Folly formulas with names
newtype NamedFormula = Named (String, F.Formula) deriving (Eq, Ord)

instance Show NamedFormula where
  show (Named (name, form)) = name ++ ": " ++ (show form)

namedFormula name form = Named (name, form)
formulaName (Named form) = fst form
formulaForm (Named form) = snd form

-- Top-level conversion functions
stringToFolly :: String -> Maybe NamedFormula
stringToFolly s = case stringToUnit s of
    Left _ -> Nothing
    Right unit -> Just (namedFormula (unitName unit) (unitFolly unit))

stringsToFolly :: [String] -> [NamedFormula]
stringsToFolly strings = catMaybes (map stringToFolly strings)

fileToFolly :: String -> IO [NamedFormula]
fileToFolly path = (readFile path) >>= (return . stringsToFolly . lines)

translateFile :: String -> IO ()
translateFile inputPath = do
    forms <- fileToFolly inputPath
    let (basePath, ext) = splitExtension inputPath
    let outputPath = basePath ++ "_folly" ++ ext
    let outputData = intercalate "\n" (map show forms)
    writeFile outputPath outputData

-- Work with TPTP Units
stringToUnit :: String -> Either String T.Unit
stringToUnit = parseUnitOnly . pack

unitName :: T.Unit -> String
unitName (T.Unit (Left (T.Atom name)) _ _) = show name
unitName (T.Unit (Right name) _ _) = show name

unitFOF :: T.Unit -> T.UnsortedFirstOrder
unitFOF (T.Unit _ (T.Formula _ (T.FOF form)) _) = form

unitFolly :: T.Unit -> F.Formula
unitFolly = fofAsFolly . unitFOF

-- Convert a TPTP FOF to Folly
fofAsFolly :: T.UnsortedFirstOrder -> F.Formula
fofAsFolly (T.Atomic (T.Predicate name terms)) = F.pr (show $ pretty name) (fofTerms terms)
fofAsFolly (T.Negated form) = F.neg (fofAsFolly form)
fofAsFolly (T.Connected f1 con f2) =
    (fofConnective con) (fofAsFolly f1) (fofAsFolly f2)
fofAsFolly (T.Quantified quant vars form) =
    quantAsFolly (fofQuantifier quant) (toList vars) (fofAsFolly form)

-- NOTE: for this to work, the system must include the equality axioms
fofAsFolly (T.Atomic (T.Equality t1 (T.Positive) t2)) =
    equals (fofTerm t1) (fofTerm t2)
fofAsFolly (T.Atomic (T.Equality t1 (T.Negative) t2)) =
    F.neg (equals (fofTerm t1) (fofTerm t2))

-- Helpers to convert various forms
quantAsFolly :: FollyQuantifier -> [(T.Var, T.Unsorted)] -> F.Formula -> F.Formula
quantAsFolly _ [] form = form
quantAsFolly quant ((T.Var var, _):vars) form =
    quant (F.var (show var)) (quantAsFolly quant vars form)

fofTerm :: T.Term -> F.Term
fofTerm (T.Function name terms) = F.func (show $ pretty name) (fofTerms terms)
fofTerm (T.Variable v) = F.var (show $ pretty v)
fofTerm (T.Number n) = F.constant (show $ pretty n)
fofTerm (T.DistinctTerm obj) = F.constant (show $ pretty obj)
fofTerms = map fofTerm

type FollyConnective = (F.Formula -> F.Formula -> F.Formula)
fofConnective :: T.Connective -> FollyConnective
fofConnective T.Conjunction = F.con
fofConnective T.Disjunction = F.dis
fofConnective T.Implication = F.imp
fofConnective T.Equivalence = F.bic
fofConnective T.ExclusiveOr = \f1 f2 -> F.neg (F.bic f1 f2)
fofConnective T.NegatedConjunction = \f1 f2 -> F.neg (F.con f1 f2)
fofConnective T.NegatedDisjunction = \f1 f2 -> F.neg (F.dis f1 f2)
fofConnective T.ReversedImplication = \f1 f2 -> F.imp (F.neg f1) (F.neg f2)

type FollyQuantifier = (F.Term -> F.Formula -> F.Formula)
fofQuantifier :: T.Quantifier -> FollyQuantifier
fofQuantifier T.Forall = F.fa
fofQuantifier T.Exists = F.te
