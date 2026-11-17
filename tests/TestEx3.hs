-- Tests for challenge 3. Run from the repo root: runghc -i. tests/TestEx3.hs

import Ex3

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
