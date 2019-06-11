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

inputFile = "./data/mizar_statements.txt"
outputFile = "./data/mizar_statements_folly.txt"

main = do
  content <- readFile inputFile
  let statementStrings = lines content
  let folly_statements = map translate statementStrings
  writeFile outputFile (concatMap stringify folly_statements)

translate :: String -> Maybe String
translate s = case (parse namedFormula "" s) of
    Left _ -> Nothing
    Right (name, form) -> Just form
translate _ = Nothing


type StringParser = Parsec String ()

-- Language definition and lexer
quantNames = ["?", "!"]
binaryConnectiveNames = ["<=>", "=>", "&", "|"]
connectiveNames = "~" : binaryConnectiveNames

language =
    PT.LanguageDef {
        PT.commentStart = "/*",
        PT.commentEnd = "*/",
        PT.commentLine = "%",
        PT.nestedComments = False,
        PT.identStart = alphaNum <|> char '_' :: StringParser Char,
        PT.identLetter = alphaNum <|> char '_' :: StringParser Char,
        PT.opStart = oneOf $ map head connectiveNames,
        PT.opLetter = oneOf $ foldr1 List.union connectiveNames,
        PT.reservedNames = [],
        PT.reservedOpNames = connectiveNames,
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

-- Expression parser
-- TODO: remove traces
func = parserTraced "Func" (
    (\fname args -> printf "%s[%s]" fname (List.intercalate ", " args)) <$>
        ident <*> (parens $ commaSep1 $ term) :: StringParser String
    )
term = parserTraced "Term" (try func <|> ident)

logicFormula = parserTraced "LogicForm"
    (unaryFormula <|> try binaryFormula <|> unitaryFormula)
unitFormula = parserTraced "UnitForm" (unitaryFormula <|> unaryFormula)
unitaryFormula = parserTraced "UnitaryForm"
    (quantifiedFormula <|> term <|> parens logicFormula)

unaryFormula = parserTraced "UnaryForm"
    (format2 <$> (symbol "~") <*> unitFormula)

binaryFormula = parserTraced "BinaryForm" (
    format3 <$> unitFormula <*> binaryConnective <*> unitFormula)
binaryConnective = choice (map symbol binaryConnectiveNames)

quantifier = (\q -> case q of
                "!" -> "V"
                "?" -> "E"
                _ -> error "Invalid quant")
        <$> choice (map symbol quantNames)

translateQuantified :: String -> [String] -> String -> String -> String
translateQuantified q vars _ form =
    (concat $ map (\v -> printf "%s %s. " q v) vars) ++ form

quantifiedFormula = parserTraced "Quantified" (
    translateQuantified <$>
        quantifier <*> (brackets $ commaSep1 ident) <*> colon <*> unitFormula)

equality = symbol "=" -- TODO: handle equality
