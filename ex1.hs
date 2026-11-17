{-
Author: Maheer Mirza
Student ID: mmm3g24
Copyright: University of Southampton 2025
Description: Regular expression bounded language generator
-}

import Data.Maybe (fromMaybe)

-- Data type definitions
data LRegExp = Let [(Int,RegExp)] RegExp
    deriving (Eq,Show,Read)

data RegExp = S SRegExp | Star SRegExp | Plus SRegExp | Seq [RegExp]
    deriving (Eq,Show,Read)

data SRegExp = Empty | Lit Char | Opt Char | Any | OptAny | SSeq [SRegExp] | Choice [SRegExp] | Var Int
    deriving (Eq,Show,Read)

-- | Generate all symbolic strings up to length k accepted by the regex
boundedLang :: Int -> LRegExp -> [String]
boundedLang bound (Let bindings mainExpr) = expandRegExp expandedBindings mainExpr
    where
        -- Resolve all variable bindings eagerly to avoid circular dependencies
        expandedBindings :: [(Int, [String])]
        expandedBindings = [(n, expandRegExp expandedBindings expr) | (n, expr) <- bindings]
        
        -- Expand a top-level RegExp to its string language
        expandRegExp :: [(Int, [String])] -> RegExp -> [String]
        expandRegExp env expr = case expr of
            S sregexp -> expandSRegExp env sregexp
            Star sregexp -> expandStar env sregexp
            Plus sregexp -> expandPlus env sregexp
            Seq exprs -> expandSeq env exprs
        
        -- Expand a simple regex to its string language
        expandSRegExp :: [(Int, [String])] -> SRegExp -> [String]
        expandSRegExp env sregexp = case sregexp of
            Empty -> [""]
            Lit c -> [[c]]
            Any -> ["."]
            OptAny -> ["", "."]
            Opt c -> ["", [c]]
            SSeq sexprs -> expandSSeq env sexprs
            Choice sexprs -> expandChoice env sexprs
            Var n -> fromMaybe [] (lookup n env)
        
        -- Expand Plus (one or more repetitions) using iterative generation
        expandPlus :: [(Int, [String])] -> SRegExp -> [String]
        expandPlus env sregexp = base ++ generateReps base
            where
                base = expandSRegExp env sregexp
                -- Recursively generate longer strings by concatenating with base
                -- Stops when all generated strings exceed the bound
                generateReps :: [String] -> [String]
                generateReps prev =
                    let next = [s1 ++ s2 | s1 <- prev, s2 <- base, length (s1 ++ s2) <= bound]
                    in if null next then [] else next ++ generateReps next
        
        -- Star is zero or more: empty string plus all Plus results
        expandStar :: [(Int, [String])] -> SRegExp -> [String]
        expandStar env sregexp = "" : expandPlus env sregexp
        
        -- Expand sequence of simple regexes using Cartesian product
        expandSSeq :: [(Int, [String])] -> [SRegExp] -> [String]
        expandSSeq _ [] = [""]
        expandSSeq env sexprs =
            let allStrings = [expandSRegExp env se | se <- sexprs]
            in [concat strs | strs <- sequence allStrings, length (concat strs) <= bound]
        
        -- Expand sequence of top-level regexes
        expandSeq :: [(Int, [String])] -> [RegExp] -> [String]
        expandSeq env exprs =
            let allStrings = [expandRegExp env e | e <- exprs]
            in [concat strs | strs <- sequence allStrings, length (concat strs) <= bound]
        
        -- Choice is union of all alternatives
        expandChoice :: [(Int, [String])] -> [SRegExp] -> [String]
        expandChoice env = concatMap (expandSRegExp env)

-- ============================================================================
-- TEST SUITE 1
-- ============================================================================

main :: IO ()
main = do
    putStrLn "=== Challenge 1: Bounded Language Generation Tests ===\n"
    
    testCase "Single literal" 3 (Let [] (S (Lit 'A'))) ["A"]
    testCase "Empty expression" 3 (Let [] (S Empty)) [""]
    testCase "Star any" 3 (Let [] (Star Any)) ["",".","..","..."]
    testCase "A* bounded to 3" 3 (Let [] (Star (Lit 'A'))) ["","A","AA","AAA"]
    testCase "A+ bounded to 3" 3 (Let [] (Plus (Lit 'A'))) ["A","AA","AAA"]
    testCase "A|B bounded to 1" 1 (Let [] (S (Choice [Lit 'A', Lit 'B']))) ["A","B"]
    testCase "AB bounded to with Opt A" 2 (Let [] (S (SSeq [Opt 'A', Lit 'B']))) ["B","AB"]
    testCase "A? bounded to 1" 1 (Let [] (S (Opt 'A'))) ["","A"]
    testCase "(AB)* bounded to 4" 4 (Let [] (Star (SSeq [Lit 'A', Lit 'B']))) ["","AB","ABAB"]
    testCase "(AB|CD) bounded to 2" 2 (Let [] (S (Choice [SSeq [Lit 'A', Lit 'B'], SSeq [Lit 'C', Lit 'D']]))) ["AB","CD"]
    testCase "let x1 = A in x1* bounded to 3" 3 (Let [(1, S (Lit 'A'))] (Star (Var 1))) ["","A","AA","AAA"]
    testCase "let x1 = A in let x2 = B in x1x2 bounded to 2" 2 
        (Let [(1, S (Lit 'A')), (2, S (Lit 'B'))] (S (SSeq [Var 1, Var 2]))) ["AB"]
    testCase "(A|B)* bounded to 3" 3 (Let [] (Star (Choice [Lit 'A', Lit 'B']))) ["","A","B","AA","AB","BA","BB","AAA","AAB","ABA","ABB","BAA","BAB","BBA","BBB"]
    
    putStrLn "\n=== All Challenge 1 tests complete ===\n"

testCase :: String -> Int -> LRegExp -> [String] -> IO ()
testCase description bound expr expected = do
    let result = boundedLang bound expr
    let status = if result == expected then "PASS" else "FAIL"
    putStrLn $ "Test: " ++ description
    putStrLn $ "Expression: " ++ show expr
    putStrLn $ "Bound: " ++ show bound
    putStrLn $ "Expected: " ++ show expected
    putStrLn $ "Output: " ++ show result
    putStrLn $ "Status: " ++ status
    putStrLn ""