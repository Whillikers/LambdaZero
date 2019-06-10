import qualified Data.List.NonEmpty as DLNE

inputFile = "./statements.txt"
outputFile = "./statements_folly.txt"

main = do
  content <- readFile inputFile
  let statementStrings = lines content
  let folly_statements = map translate statementStrings
  writeFile outputFile (concatMap stringify folly_statements)


failureString = "failure"

translate :: String -> Maybe String
translate stringFormula = putBackName name formula
  where
    delStr :: Eq a => [a] -> Maybe [a] -> Maybe [a]
    delStr m Nothing = Nothing
    delStr m (Just s) = let l = length m in
      if take l s == m then Just (drop l s) else Nothing

    getName = fmap (break (\c -> c == ','))

    delAxiom x = case x of
      Just c  -> if (take 5 c) == "axiom" then Just $ drop 5 c else Nothing
      Nothing -> Nothing

    getVars x = (fmap (map delSpaces)
                 $ (delSplit ',' ((mRevOrder (delStr "]")) s)),
                 (delStr "]") r)
      where (s, r) = DLNE.unzip $ fmap (break (\c -> c == ']')) $ (delStr "[") x
    
    putBackName n r = case (n, r) of
      (Nothing, _)     -> Nothing
      (_, Nothing)     -> Nothing
      (Just n, Just r) -> Just $ n ++ " = " ++ r

    (name, rest1) = (DLNE.unzip
                   . getName
                   . delParens
                   . (mRevOrder (delStr "."))
                   . (delStr "fof")) (Just stringFormula)

    -- Currently "vars" does nothing, but contains a list of the strings in the
    -- rest of the formula.
    (vars, rest2) = getVars $ mDelSpaces $ (delStr "!") $ delParens
      $ (delStr ",") $ delAxiom $ mDelSpaces $ (delStr ",") $ rest1

    formula = mDelSpaces $ (delStr ":") $ mDelSpaces $ rest2

delSplit :: Eq a => a -> Maybe [a] -> Maybe [[a]]
delSplit a Nothing = Nothing
delSplit a (Just toSplit) = Just $ delSplitHelp ([], [], toSplit)
  where delSplitHelp (completed, curr, []) = reverse completed
        delSplitHelp (completed, curr, (b : bs))
          | a == b    = delSplitHelp ((reverse curr) : completed, [], bs)
          | otherwise = delSplitHelp (completed, b : curr, bs)

revOrder f = reverse . f . reverse

mRevOrder f = (fmap reverse) . f . (fmap reverse)

mDelSpaces :: Maybe String -> Maybe String
mDelSpaces = fmap delSpaces

delSpaces :: String -> String
delSpaces = ((revOrder (dropWhile isSpace)) . (dropWhile isSpace))
  where isSpace = (==) ' '

delParens :: Maybe String -> Maybe String
delParens = (fmap delSpaces) . (fmap (\s -> let d = delSpaces s in
                                              if (head d == '(')
                                                 && (last d == ')')
                                              then drop 1 $ (revOrder (drop 1)) d
                                              else failureString))
      
stringify x = case x of
  Just s -> s ++ "\n"
  Nothing -> failureString ++ "\n"
