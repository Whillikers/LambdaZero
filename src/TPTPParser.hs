-- Parses the subset of the TPTP language needed for FOL Mizar statements
-- See: http://www.tptp.org/TPTP/SyntaxBNF.html

module TPTPParser where

import qualified Data.List as List
import Data.Maybe
import Data.Either

import Text.Printf

import Text.Parsec
import Text.Parsec.Language
import Text.Parsec.Expr
import qualified Text.Parsec.Token as PT

import qualified Folly.Formula as F
import Folly.Formula (Term (..), Formula (..))

inputFile = "./data/mizar_examples.txt"
-- inputFile = "./data/mizar_statements.txt"
outputFile = "./data/mizar_statements_folly.txt"

main = do
  content <- readFile inputFile
  let statementStrings = lines content
  let folly_statements = mapMaybe translate statementStrings
  writeFile outputFile (concatMap show folly_statements)

translate :: String -> Maybe Formula
translate s = case (parse namedFormula "" s) of
    Left _ -> Nothing
    Right (name, role, form) -> Just form


type StringParser = Parsec String ()

-- Language definition and lexer
quantNames = ["?", "!"]
binaryConnectiveNames = ["<=>", "=>", "&", "|"]
connectiveNames = "~" : binaryConnectiveNames
opNames = quantNames ++ connectiveNames

language =
    PT.LanguageDef {
        PT.commentStart = "/*",
        PT.commentEnd = "*/",
        PT.commentLine = "%",
        PT.nestedComments = False,
        PT.identStart = alphaNum <|> char '_' :: StringParser Char,
        PT.identLetter = alphaNum <|> char '_' :: StringParser Char,
        PT.opStart = oneOf $ map head opNames,
        PT.opLetter = oneOf $ foldr1 List.union opNames,
        PT.reservedNames = [],
        PT.reservedOpNames = opNames,
        PT.caseSensitive = True
    }

lexer = PT.makeTokenParser language
ident = PT.identifier lexer
symbol = PT.symbol lexer
lexeme = PT.lexeme lexer
parens = PT.parens lexer
brackets = PT.brackets lexer
comma = PT.comma lexer
colon = PT.colon lexer
commaSep = PT.commaSep lexer
commaSep1 = PT.commaSep1 lexer
dot = PT.dot lexer
reservedOp = PT.reservedOp lexer

-- Formatting helpers
format2 = printf "%s %s" :: String -> String -> String
format3 = printf "%s %s %s" :: String -> String -> String -> String

-- Main parser
namedFormula :: StringParser (String, String, Formula)
namedFormula = do
    symbol "fof"
    (name, role, form) <- parens (do
        name <- ident
        comma
        role <- ident
        comma
        form <- parens logicFormula
        return (name, role, form))
    dot
    eof
    return (name, role, form)

-- Expression parser
-- logicFormula = unaryFormula <|> try binaryFormula <|> unitaryFormula :: StringParser Formula
logicFormula = unaryFormula <|> unitaryFormula :: StringParser Formula
unitFormula = unitaryFormula <|> unaryFormula :: StringParser Formula
unitaryFormula = quantifiedFormula <|> atomicFormula <|> parens logicFormula :: StringParser Formula

atomicFormula = try atomicPredicate <|> atomicConstant
atomicConstant = (flip F.pr []) <$> ident
atomicPredicate = do
    predicate <- ident
    args <- parens (commaSep1 term)
    return (F.pr predicate args)


term = try func <|> constant :: StringParser Term
func = F.func <$> ident <*> (parens $ commaSep1 $ term) :: StringParser Term
variable = F.var <$> ident :: StringParser Term
constant = F.constant <$> ident :: StringParser Term

unaryFormula :: StringParser Formula
unaryFormula = do
    reservedOp "~"
    form <- unitFormula
    return (F.neg form)

-- TODO: parse binary connectives
-- binaryFormula = format3 <$> unitFormula <*> binaryConnective <*> unitFormula
-- binaryConnective = choice (map symbol binaryConnectiveNames)

quantifier = (\q -> case q of
                "!" -> F.fa
                "?" -> F.te
                _ -> error "Invalid quantifier")
        <$> choice (map symbol quantNames)

quantifiedFormula = do
    quant <- quantifier
    vars <- brackets (commaSep1 variable)
    colon
    form <- unitFormula
    return (makeQuantified quant vars form)
      where
        makeQuantified :: (F.Term -> F.Formula -> F.Formula) ->
            [F.Term] -> F.Formula -> F.Formula
        makeQuantified _ [] form = form
        makeQuantified q (vars : rest) f = q vars (makeQuantified q rest f)

-- equality = symbol "=" -- TODO: handle equality
