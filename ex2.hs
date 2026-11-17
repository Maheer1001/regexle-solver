{-
Author: Maheer Mirza
Student ID: mmm3g24
Copyright: University of Southampton 2025
Description: Regular expression parser
-}

import Data.Maybe (fromMaybe)
import Data.Char (isAsciiUpper, isDigit)

-- Data type definitions
data LRegExp = Let [(Int, RegExp)] RegExp
    deriving (Eq, Show, Read)

data RegExp = S SRegExp | Star SRegExp | Plus SRegExp | Seq [RegExp]
    deriving (Eq, Show, Read)

data SRegExp = Empty | Lit Char | Opt Char | Any | OptAny | SSeq [SRegExp] | Choice [SRegExp] | Var Int
    deriving (Eq, Show, Read)

-- Token representation for intermediate parsing
data Token = 
    Special Char
    | LitChar Char
    | Bracket Char
    | Wildcard
    | EmptyToken
    | OptionalToken
    | StarWrapped Token
    | PlusWrapped Token
    | SequenceWrapped [Token]
    | ChoiceWrapped [Token]
    | OptChar Char
    | VarToken Int
    | LetToken
    | InToken
    | EqualsToken
    deriving (Eq, Show, Read)

-- | Parse a regular expression string into an LRegExp AST
-- Returns Nothing for invalid input, Just LRegExp otherwise
parseRegExp :: String -> Maybe LRegExp
parseRegExp "" = Nothing
parseRegExp input =
    case parseTopLevel (tokenise input) of
        Just (lregexp, []) -> Just lregexp
        _ -> Nothing

-- Parse top-level LRegExp (handles let bindings)
parseTopLevel :: [Token] -> Maybe (LRegExp, [Token])
parseTopLevel tokens =
    case parseLetBindings tokens [] of
        Just (bindings, remaining) ->
            case pipeline remaining of
                [token] -> Just (Let bindings (tokenToRegExp token), [])
                _ -> Nothing
        Nothing -> Nothing

-- Parse let bindings recursively, accumulating variable bindings
parseLetBindings :: [Token] -> [(Int, RegExp)] -> Maybe ([(Int, RegExp)], [Token])
parseLetBindings (LetToken : tokens) accumulator =
    case tokens of
        (VarToken n : EqualsToken : rest) ->
            case parseUntilIn rest [] of
                Just (exprTokens, InToken : remaining) ->
                    case pipeline exprTokens of
                        [token] ->
                            let binding = (n, tokenToRegExp token)
                            in parseLetBindings remaining (accumulator ++ [binding])
                        _ -> Nothing
                _ -> Nothing
        _ -> Nothing
parseLetBindings tokens accumulator = Just (accumulator, tokens)

-- Parse tokens until 'in' keyword is found
parseUntilIn :: [Token] -> [Token] -> Maybe ([Token], [Token])
parseUntilIn [] _ = Nothing
parseUntilIn (InToken : rest) accumulator = Just (reverse accumulator, InToken : rest)
parseUntilIn (token : rest) accumulator = parseUntilIn rest (token : accumulator)

-- Tokenise input string into intermediate Token representation
tokenise :: String -> [Token]
tokenise [] = []
tokenise ('l':'e':'t':' ':rest) = LetToken : tokenise (dropWhile (== ' ') rest)
tokenise (' ':'i':'n':' ':rest) = InToken : tokenise (dropWhile (== ' ') rest)
tokenise ('x':rest)
    | not (null digits) = VarToken (read digits) : tokenise remaining
    where
        (digits, remaining) = span isDigit rest
tokenise ('=':rest) = EqualsToken : tokenise rest
tokenise (character:rest)
    | character == ' ' = tokenise rest
    | character `elem` "|*+" = Special character : tokenise rest
    | character `elem` "()" = Bracket character : tokenise rest
    | character == '.' = Wildcard : tokenise rest
    | character == 'e' = EmptyToken : tokenise rest
    | character == '?' = OptionalToken : tokenise rest
    | isAsciiUpper character =
        case rest of
            ('?':rest2) -> OptChar character : tokenise rest2
            _ -> LitChar character : tokenise rest
    | otherwise = []  -- Invalid character returns empty list, leading to Nothing

-- Processing pipeline: tokenise -> processOperators -> optimise
pipeline :: [Token] -> [Token]
pipeline = optimiseStructure . processOperators

-- Process operators (*, +, |) and group with appropriate precedence
processOperators :: [Token] -> [Token]
processOperators tokens = processChoiceOperator (processStarAndPlus tokens)
    where
        -- Handle star (*) and plus (+) operators with highest precedence
        processStarAndPlus :: [Token] -> [Token]
        processStarAndPlus [] = []
        processStarAndPlus (LitChar character : Special '*' : rest) =
            StarWrapped (LitChar character) : processStarAndPlus rest
        processStarAndPlus (LitChar character : Special '+' : rest) =
            PlusWrapped (LitChar character) : processStarAndPlus rest
        processStarAndPlus (OptChar character : Special '*' : rest) =
            StarWrapped (OptChar character) : processStarAndPlus rest
        processStarAndPlus (OptChar character : Special '+' : rest) =
            PlusWrapped (OptChar character) : processStarAndPlus rest
        processStarAndPlus (VarToken n : Special '*' : rest) =
            StarWrapped (VarToken n) : processStarAndPlus rest
        processStarAndPlus (VarToken n : Special '+' : rest) =
            PlusWrapped (VarToken n) : processStarAndPlus rest
        processStarAndPlus (Bracket '(' : rest) =
            let (groupedContent, remaining) = collectUntilClosingBracket rest 0 []
                processedGroup = processStarAndPlus groupedContent
                wrappedGroup = wrapIfMultiple processedGroup
            in case remaining of
                (Special '*' : rest2) -> StarWrapped wrappedGroup : processStarAndPlus rest2
                (Special '+' : rest2) -> PlusWrapped wrappedGroup : processStarAndPlus rest2
                _ -> wrappedGroup : processStarAndPlus remaining
        processStarAndPlus (token : rest) = token : processStarAndPlus rest

        -- Collect tokens until matching closing bracket, tracking nesting depth
        collectUntilClosingBracket :: [Token] -> Int -> [Token] -> ([Token], [Token])
        collectUntilClosingBracket [] _ accumulator = (reverse accumulator, [])
        collectUntilClosingBracket (Bracket ')' : rest) 0 accumulator =
            (reverse accumulator, rest)
        collectUntilClosingBracket (Bracket ')' : rest) depth accumulator =
            collectUntilClosingBracket rest (depth - 1) (Bracket ')' : accumulator)
        collectUntilClosingBracket (Bracket '(' : rest) depth accumulator =
            collectUntilClosingBracket rest (depth + 1) (Bracket '(' : accumulator)
        collectUntilClosingBracket (token : rest) depth accumulator =
            collectUntilClosingBracket rest depth (token : accumulator)

        -- Wrap single token as-is, multiple tokens in SequenceWrapped
        wrapIfMultiple :: [Token] -> Token
        wrapIfMultiple [singleToken] = singleToken
        wrapIfMultiple multipleTokens = SequenceWrapped multipleTokens

        -- Handle choice (|) operator with lowest precedence
        processChoiceOperator :: [Token] -> [Token]
        processChoiceOperator = processTokens
            where
                processTokens [] = []
                processTokens allTokens
                    | hasChoiceOperator allTokens =
                        [ChoiceWrapped (splitAndProcessByChoice allTokens)]
                    | otherwise = map recursivelyProcessToken allTokens

                hasChoiceOperator [] = False
                hasChoiceOperator (Special '|' : _) = True
                hasChoiceOperator (_ : rest) = hasChoiceOperator rest

                splitAndProcessByChoice toks =
                    map (wrapSingleOrSequence . processChoiceOperator) (splitByPipe toks)
                    where
                        wrapSingleOrSequence [single] = single
                        wrapSingleOrSequence multiple = SequenceWrapped multiple

                -- Split tokens by pipe operator, creating list of alternatives
                splitByPipe [] = [[]]
                splitByPipe (Special '|' : rest) = [] : splitByPipe rest
                splitByPipe (token : rest) =
                    case splitByPipe rest of
                        (first:others) -> (token : first) : others
                        [] -> [[token]]

                recursivelyProcessToken (StarWrapped content) =
                    StarWrapped (head (processChoiceOperator [content]))
                recursivelyProcessToken (PlusWrapped content) =
                    PlusWrapped (head (processChoiceOperator [content]))
                recursivelyProcessToken (SequenceWrapped items) =
                    SequenceWrapped (processChoiceOperator items)
                recursivelyProcessToken token = token

-- Optimise token structure: group literals, merge sequences, flatten choices
optimiseStructure :: [Token] -> [Token]
optimiseStructure = flattenNestedChoices . mergeAdjacentSequences . groupConsecutiveLiterals
    where
        -- Group consecutive literal characters into sequences
        groupConsecutiveLiterals :: [Token] -> [Token]
        groupConsecutiveLiterals = go []
            where
                go accumulator [] = flushAccumulator accumulator
                go accumulator (LitChar character : rest) =
                    go (LitChar character : accumulator) rest
                go accumulator (OptChar character : rest) =
                    flushAccumulator accumulator ++ [OptChar character] ++ go [] rest
                go accumulator (VarToken n : rest) =
                    flushAccumulator accumulator ++ [VarToken n] ++ go [] rest
                go accumulator (StarWrapped content : rest) =
                    flushAccumulator accumulator ++
                    [StarWrapped (head (groupConsecutiveLiterals [content]))] ++
                    go [] rest
                go accumulator (PlusWrapped content : rest) =
                    flushAccumulator accumulator ++
                    [PlusWrapped (head (groupConsecutiveLiterals [content]))] ++
                    go [] rest
                go accumulator (ChoiceWrapped alternatives : rest) =
                    flushAccumulator accumulator ++
                    [ChoiceWrapped (map (head . groupConsecutiveLiterals . (:[])) alternatives)] ++
                    go [] rest
                go accumulator (other : rest) =
                    flushAccumulator accumulator ++ [other] ++ go [] rest

                flushAccumulator [] = []
                flushAccumulator [single] = [single]
                flushAccumulator multiple = [SequenceWrapped (reverse multiple)]

        -- Merge adjacent SequenceWrapped tokens and flatten nested sequences
        mergeAdjacentSequences :: [Token] -> [Token]
        mergeAdjacentSequences [] = []
        mergeAdjacentSequences tokens =
            let processed = go tokens
            in case processed of
                [single@(StarWrapped _)] -> [single]
                [single@(PlusWrapped _)] -> [single]
                [single@(ChoiceWrapped _)] -> [single]
                [single@(SequenceWrapped _)] -> [single]
                [single] -> [single]
                multiple -> [SequenceWrapped multiple]
            where
                go [] = []
                go (SequenceWrapped first : SequenceWrapped second : rest) =
                    go (SequenceWrapped (first ++ second) : rest)
                go (SequenceWrapped items : rest) =
                    SequenceWrapped (flattenSequences items) : go rest
                go (StarWrapped content : rest) =
                    StarWrapped (head (mergeAdjacentSequences [content])) : go rest
                go (PlusWrapped content : rest) =
                    PlusWrapped (head (mergeAdjacentSequences [content])) : go rest
                go (ChoiceWrapped alternatives : rest) =
                    ChoiceWrapped (map (head . mergeAdjacentSequences . (:[])) alternatives) : go rest
                go (token : rest) = token : go rest

                flattenSequences [] = []
                flattenSequences (SequenceWrapped [single] : rest) = single : flattenSequences rest
                flattenSequences (SequenceWrapped items : rest) = items ++ flattenSequences rest
                flattenSequences (other : rest) = other : flattenSequences rest

        -- Flatten nested choices and remove duplicates
        flattenNestedChoices :: [Token] -> [Token]
        flattenNestedChoices = map flattenToken
            where
                flattenToken (ChoiceWrapped alternatives) =
                    ChoiceWrapped (removeDuplicates (concatMap extractAlternatives alternatives))
                flattenToken (StarWrapped content) = StarWrapped (head (flattenNestedChoices [content]))
                flattenToken (PlusWrapped content) = PlusWrapped (head (flattenNestedChoices [content]))
                flattenToken (SequenceWrapped items) = SequenceWrapped (flattenNestedChoices items)
                flattenToken token = token

                extractAlternatives (ChoiceWrapped innerAlternatives) =
                    concatMap extractAlternatives innerAlternatives
                extractAlternatives other = [flattenToken other]

                removeDuplicates [] = []
                removeDuplicates (first:rest) = first : removeDuplicates (filter (/= first) rest)

-- Convert Token to RegExp, handling operator precedence levels
tokenToRegExp :: Token -> RegExp
tokenToRegExp (StarWrapped content) = Star (tokenToSRegExp content)
tokenToRegExp (PlusWrapped content) = Plus (tokenToSRegExp content)
tokenToRegExp (SequenceWrapped items) =
    if any isRegExpLevelOperator items
        then Seq (map tokenToRegExp items)
        else S (tokenToSRegExp (SequenceWrapped items))
    where
        isRegExpLevelOperator (StarWrapped _) = True
        isRegExpLevelOperator (PlusWrapped _) = True
        isRegExpLevelOperator _ = False
tokenToRegExp token = S (tokenToSRegExp token)

-- Convert Token to SRegExp, removing Empty tokens from sequences
tokenToSRegExp :: Token -> SRegExp
tokenToSRegExp (LitChar character) = Lit character
tokenToSRegExp (OptChar character) = Opt character
tokenToSRegExp (VarToken n) = Var n
tokenToSRegExp Wildcard = Any
tokenToSRegExp EmptyToken = Empty
tokenToSRegExp (SequenceWrapped items) =
    let converted = map tokenToSRegExp (filter isNotEmpty items)
    in case converted of
        [] -> Empty
        [single] -> single
        multiple -> SSeq multiple
    where
        isNotEmpty EmptyToken = False
        isNotEmpty _ = True
tokenToSRegExp (ChoiceWrapped alternatives) =
    let converted = map tokenToSRegExp alternatives
        flattened = concatMap flattenIfChoice converted
    in case flattened of
        [] -> Empty
        [single] -> single
        multiple -> Choice multiple
    where
        flattenIfChoice (Choice inner) = inner
        flattenIfChoice other = [other]
tokenToSRegExp (StarWrapped _) = error "Star operator must be at RegExp level"
tokenToSRegExp (PlusWrapped _) = error "Plus operator must be at RegExp level"
tokenToSRegExp _ = error "Invalid token in SRegExp context"

-- ============================================================================
-- TEST SUITE 2
-- ============================================================================

main :: IO ()
main = do
    putStrLn "=== Challenge 2: Regular Expression Parser Tests ===\n"

    testCase "" Nothing
    testCase "A*" (Just (Let [] (Star (Lit 'A'))))
    testCase "(AB)*" (Just (Let [] (Star (SSeq [Lit 'A', Lit 'B']))))
    testCase "A|B" (Just (Let [] (S (Choice [Lit 'A', Lit 'B']))))
    testCase "(A|B)*" (Just (Let [] (Star (Choice [Lit 'A', Lit 'B']))))
    testCase "ABC" (Just (Let [] (S (SSeq [Lit 'A', Lit 'B', Lit 'C']))))
    testCase "(AB)(CD)" (Just (Let [] (S (SSeq [Lit 'A', Lit 'B', Lit 'C', Lit 'D']))))
    testCase "AB|CD" (Just (Let [] (S (Choice [SSeq [Lit 'A', Lit 'B'], SSeq [Lit 'C', Lit 'D']]))))
    testCase "(AB(CD)(EF|(GH|KL)))*" 
        (Just (Let [] (Star (SSeq [Lit 'A', Lit 'B', Lit 'C', Lit 'D', 
                                    Choice [SSeq [Lit 'E', Lit 'F'], 
                                    SSeq [Lit 'G', Lit 'H'], 
                                    SSeq [Lit 'K', Lit 'L']]]))))
    testCase "(AB)+" (Just (Let [] (Plus (SSeq [Lit 'A', Lit 'B']))))
    testCase "A*B+C" (Just (Let [] (Seq [Star (Lit 'A'), Plus (Lit 'B'), S (Lit 'C')])))
    testCase "A?" (Just (Let [] (S (Opt 'A'))))
    testCase "let x1 = A in x1" (Just (Let [(1, S (Lit 'A'))] (S (Var 1))))
    testCase "let x1 = A in let x2 = B in x1x2" 
        (Just (Let [(1, S (Lit 'A')), (2, S (Lit 'B'))] (S (SSeq [Var 1, Var 2]))))

    putStrLn "\n=== All Challenge 2 tests complete ===\n"

testCase :: String -> Maybe LRegExp -> IO ()
testCase input expected = do
    let result = parseRegExp input
    let status = if result == expected then "PASS" else "FAIL"
    putStrLn $ "Test: " ++ show input
    putStrLn $ "Expected: " ++ show expected
    putStrLn $ "Output:   " ++ show result
    putStrLn $ "Status:   " ++ status
    putStrLn ""