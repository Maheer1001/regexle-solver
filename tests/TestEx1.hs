-- Tests for challenge 1. Run from the repo root: runghc -i. tests/TestEx1.hs

import Ex1

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
