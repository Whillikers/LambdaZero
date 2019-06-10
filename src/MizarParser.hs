module MizarParser where

import qualified Data.List as List
import qualified Data.List.NonEmpty as DLNE
import Data.Maybe
import Data.Either
import Text.Parsec

inputFile = "./data/mizar_statements.txt"
outputFile = "./data/mizar_statements_folly.txt"

main = do
  content <- readFile inputFile
  let statementStrings = lines content
  let folly_statements = map translate statementStrings
  return (statementStrings !! 20916)
  -- writeFile outputFile (concatMap stringify folly_statements)

translate :: String -> Maybe String
translate s = case (parse namedFormula "" s) of
    Left _ -> Nothing
    Right (name, form) -> Just form


type StringParser = Parsec String () String

namedFormula :: Parsec String () (String, String)
namedFormula = do
    string "fof("
    name <- manyTill anyChar (string ", ")
    string "axiom,"
    skipMany1 (char ' ')
    form <- formula
    string " )."
    eof
    return (name, form)

formula :: StringParser
formula = do
    parserTrace "form"
    optional (char '(')
    out <- neg <|> quantifier <|> connective <|> function
    optional (char ')')
    return $ "(" ++ out ++ ")"


term = (parserTrace "term") >> (try function <|> name) :: StringParser

function :: StringParser
function = do
    parserTrace "func"
    fname <- name
    char '('
    args <- commaSeparated term
    char ')'
    return $ fname ++ "[" ++ (List.intercalate ", " args) ++ "]"

neg :: StringParser
neg = do
    string " ~ "
    form <- formula
    return $ "~" ++ form

connective :: StringParser
connective = do
    parserTrace "conn"
    form1 <- formula
    skipMany1 (char ' ')
    conn <- choice [string "=>", string "&", string "|", string "<=>"]
    skipMany1 (char ' ')
    form2 <- formula
    let conn_folly = case conn of "=>" -> " -> "
                                  "&" -> " & "
                                  "|" -> " | "
                                  "<=>" -> " <-> "
    return $ form1 ++ conn ++ form2

quantifier :: StringParser
quantifier = do
    parserTrace "quant"
    quant <- choice [char '!', char '?']
    let quant_folly = case quant of '!' -> "V"
                                    '?' -> "E"
    string " ["
    vars <- commaSeparated name
    string "] :"
    skipMany1 (char ' ')
    form <- formula
    return $
        (concat $ (map (\v -> quant_folly ++ " " ++ v ++ ". ") vars)) ++ form

name = many1 (alphaNum <|> char '_') :: StringParser

commaSeparated :: StringParser -> Parsec String () [String]
commaSeparated p = sepBy1 p (string ", ")

-- parens = between (char '(') (char ')')

-- failureString = "failure"

-- translate :: String -> Maybe String
-- -- translate stringFormula = putBackName name formula
-- translate stringFormula = formula
--   where
--     (name, rest1) = (DLNE.unzip
--                    . getName
--                    . delParens
--                    . (mRevOrder (delStr "."))
--                    . (delStr "fof")) (Just stringFormula)

--     -- Currently "vars" does nothing, but contains a list of the strings in the
--     -- rest of the formula.
--     formula = delParens $ (delStr ",") $ delAxiom $ mDelSpaces $ (delStr ",")
--       $ rest1
--     -- (vars, rest2) = getVars $ mDelSpaces $ (delStr "!") $ delParens
--     --   $ (delStr ",") $ delAxiom $ mDelSpaces $ (delStr ",") $ rest1

--     -- formula = mDelSpaces $ (delStr ":") $ mDelSpaces $ rest2

-- delStr :: Eq a => [a] -> Maybe [a] -> Maybe [a]
-- delStr m Nothing = Nothing
-- delStr m (Just s) = let l = length m in
--   if take l s == m then Just (drop l s) else Nothing

-- getName = fmap (break (\c -> c == ','))

-- delAxiom x = case x of
--   Just c  -> if (take 5 c) == "axiom" then Just $ drop 5 c else Nothing
--   Nothing -> Nothing

-- getVars x = (fmap (map delSpaces)
--              $ (delSplit ',' ((mRevOrder (delStr "]")) s)),
--              (delStr "]") r)
--   where (s, r) = DLNE.unzip $ fmap (break (\c -> c == ']')) $ (delStr "[") x

-- putBackName n r = case (n, r) of
--   (Nothing, _)     -> Nothing
--   (_, Nothing)     -> Nothing
--   (Just n, Just r) -> Just $ n ++ " = " ++ r

-- delSplit :: Eq a => a -> Maybe [a] -> Maybe [[a]]
-- delSplit a Nothing = Nothing
-- delSplit a (Just toSplit) = Just $ delSplitHelp ([], [], toSplit)
--   where delSplitHelp (completed, curr, []) = reverse completed
--         delSplitHelp (completed, curr, (b : bs))
--           | a == b    = delSplitHelp ((reverse curr) : completed, [], bs)
--           | otherwise = delSplitHelp (completed, b : curr, bs)

-- revOrder f = reverse . f . reverse

-- mRevOrder f = (fmap reverse) . f . (fmap reverse)

-- mDelSpaces :: Maybe String -> Maybe String
-- mDelSpaces = fmap delSpaces

-- delSpaces :: String -> String
-- delSpaces = ((revOrder (dropWhile isSpace)) . (dropWhile isSpace))
--   where isSpace = (==) ' '

-- delParens :: Maybe String -> Maybe String
-- delParens = (fmap delSpaces) . (fmap (\s -> let d = delSpaces s in
--                                               if (head d == '(')
--                                                  && (last d == ')')
--                                               then drop 1 $ (revOrder (drop 1)) d
--                                               else failureString))

-- stringify x = case x of
--   Just s -> s ++ "\n"
--   Nothing -> failureString ++ "\n"
