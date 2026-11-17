-- Tests for challenge 2. Run from the repo root: runghc -i. tests/TestEx2.hs

import Ex2

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
