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

    -- Wildcards, optional characters and mixed operators
    testCase "." (Just (Let [] (S Any)))
    testCase "A.B" (Just (Let [] (S (SSeq [Lit 'A', Any, Lit 'B']))))
    testCase "e" (Just (Let [] (S Empty)))
    testCase "A?B" (Just (Let [] (S (SSeq [Opt 'A', Lit 'B']))))
    testCase "A+B?" (Just (Let [] (Seq [Plus (Lit 'A'), S (Opt 'B')])))
    testCase "(A|B)C" (Just (Let [] (S (SSeq [Choice [Lit 'A', Lit 'B'], Lit 'C']))))
    testCase "(PO|BA)(U|T)" (Just (Let [] (S (SSeq [Choice [SSeq [Lit 'P', Lit 'O'], SSeq [Lit 'B', Lit 'A']], Choice [Lit 'U', Lit 'T']]))))
    testCase "(P|L|M)+OOY" (Just (Let [] (Seq [Plus (Choice [Lit 'P', Lit 'L', Lit 'M']), S (SSeq [Lit 'O', Lit 'O', Lit 'Y'])])))

    -- Invalid input is rejected with Nothing
    testCase "a" Nothing
    testCase "x" Nothing
    testCase "let x1 = A" Nothing
    testCase "let x1 = A in" Nothing

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
