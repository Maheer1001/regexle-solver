{-
Author: Maheer Mirza
Student ID: mmm3g24
Copyright: University of Southampton 2025
Description: Regular expression unparser
-}

import Data.List (intercalate)

-- Data type definitions
data LRegExp = Let [(Int,RegExp)] RegExp
    deriving (Eq,Show,Read)

data RegExp = S SRegExp | Star SRegExp | Plus SRegExp | Seq [RegExp]
    deriving (Eq,Show,Read)

data SRegExp = Empty | Lit Char | Opt Char | Any | OptAny | SSeq [SRegExp] | Choice [SRegExp] | Var Int
    deriving (Eq,Show,Read)

-- | Convert an LRegExp back to its string representation
-- Uses minimal parentheses and flattens nested structures
unparseRegExp :: LRegExp -> String
unparseRegExp (Let [] expr) = unparseRegExp' expr
unparseRegExp (Let bindings expr) =
    unwords (map unparseBinding bindings) ++ unparseRegExp' expr
    where
        unparseBinding (var, regexp) =
            "let x" ++ show var ++ " = " ++ unparseRegExp' regexp ++ " in "

-- Unparse a RegExp to string
unparseRegExp' :: RegExp -> String
unparseRegExp' (S sregexp) = unparseSRegExp False sregexp
unparseRegExp' (Star sregexp) = unparseSRegExp True sregexp ++ "*"
unparseRegExp' (Plus sregexp) = unparseSRegExp True sregexp ++ "+"
unparseRegExp' (Seq regexps) = concatMap unparseRegExpInSeq regexps
    where
        unparseRegExpInSeq (S sregexp) = unparseSRegExp False sregexp
        unparseRegExpInSeq other = unparseRegExp' other

-- Unparse an SRegExp, adding parentheses only when necessary
-- needsParens indicates if context requires wrapping in brackets
unparseSRegExp :: Bool -> SRegExp -> String
unparseSRegExp _ Empty = "e"
unparseSRegExp _ (Lit char) = [char]
unparseSRegExp _ (Opt char) = [char, '?']
unparseSRegExp _ Any = "."
unparseSRegExp _ OptAny = ".?"
unparseSRegExp _ (Var n) = "x" ++ show n
unparseSRegExp needsParens (SSeq sregexps) =
    let cleaned = removeEmpty sregexps
        inner = concatMap unparseSRegExpInSeq cleaned
    in if needsParens && not (isSingleLiteral cleaned)
        then "(" ++ inner ++ ")"
        else inner
    where
        removeEmpty = filter (/= Empty)
        isSingleLiteral [Lit _] = True
        isSingleLiteral _ = False

        -- Add parentheses around Choice when it appears in a sequence
        unparseSRegExpInSeq (Choice alternatives) =
            "(" ++ unparseSRegExp False (Choice alternatives) ++ ")"
        unparseSRegExpInSeq other = unparseSRegExp False other

unparseSRegExp needsParens (Choice alternatives) =
    let flattened = flattenChoices alternatives
        deduplicated = removeDuplicates flattened
        unparsed = map unparseChoiceAlternative deduplicated
        inner = intercalate "|" unparsed
    in if needsParens
        then "(" ++ inner ++ ")"
        else inner
    where
        -- Flatten nested Choice constructors recursively
        flattenChoices [] = []
        flattenChoices (Choice inner : rest) = flattenChoices inner ++ flattenChoices rest
        flattenChoices (other : rest) = other : flattenChoices rest

        -- Remove duplicate alternatives from choice
        removeDuplicates [] = []
        removeDuplicates (first : rest) = first : removeDuplicates (filter (/= first) rest)

        -- Unparse each alternative, filtering out Empty from sequences
        unparseChoiceAlternative (SSeq items) = 
            concatMap (unparseSRegExp False) (filter (/= Empty) items)
        unparseChoiceAlternative other = unparseSRegExp False other

-- ============================================================================
-- TEST SUITE 3
-- ============================================================================

main :: IO ()
main = do
    putStrLn "=== Challenge 3: Unparsing Tests ===\n"
    
    testCase (Let [] (S Empty)) "e"
    testCase (Let [] (Star (SSeq [Lit 'A',Lit 'B',Lit 'C',Lit 'D',Choice [SSeq [Lit 'E',Lit 'F'],SSeq [Lit 'G',Lit 'H'],SSeq [Lit 'K',Lit 'L'],Choice [SSeq [Lit 'A']]]])))
        "(ABCD(EF|GH|KL|A))*"
    testCase (Let [] (S (Lit 'A'))) "A"
    testCase (Let [] (Star (Lit 'A'))) "A*"
    testCase (Let [] (S (Choice [Lit 'A', Lit 'B']))) "A|B"
    testCase (Let [] (Seq [Star (Lit 'A'), Plus (Lit 'B'), S (Lit 'C')])) "A*B+C"
    testCase (Let [] (S (SSeq [Lit 'A', Lit 'B', Lit 'C']))) "ABC"
    testCase (Let [] (S (Choice [Lit 'A', Lit 'B', Lit 'A']))) "A|B"
    testCase (Let [] (S (SSeq [Lit 'A', Empty, Lit 'B']))) "AB"
    testCase (Let [(1, S (Lit 'A'))] (Star (Var 1))) "let x1 = A in x1*"
    
    putStrLn "\n=== All Challenge 3 tests complete ===\n"

testCase :: LRegExp -> String -> IO ()
testCase input expected = do
    let result = unparseRegExp input
    let status = if result == expected then "PASS" else "FAIL"
    putStrLn $ "Test: " ++ show input
    putStrLn $ "Expected: " ++ show expected
    putStrLn $ "Output:   " ++ show result
    putStrLn $ "Status:   " ++ status
    putStrLn ""