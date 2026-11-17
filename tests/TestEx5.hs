-- Tests for challenge 5. Run from the repo root: runghc -i. tests/TestEx5.hs

import Ex5

main :: IO ()
main = do
    putStrLn "=== Challenge 5: Hexagonal Regexle Solver Tests ===\n"
    
    testCase "Size 1 Hexagon" "A"
        [ Let [] (Plus (Lit 'A'))
        , Let [] (Plus (Lit 'A'))
        , Let [] (Plus (Lit 'A'))
        ]
    
    testCase "Size 3 Hexagon" "HTDAWSZ"
        [ Let [] (Plus (Choice [Lit 'H', Lit 'T', Lit 'U', Lit 'V']))
        , Let [] (Seq [S (Lit 'D'), S (SSeq [Lit 'A', Lit 'W']), Star Any])
        , Let [] (Plus (Choice [Lit 'Z', Lit 'F', Lit 'M', Lit 'S']))
        , Let [] (Seq [Star Any, S (Lit 'S'), Star Any])
        , Let [] (Seq [Star Any, S (SSeq [Lit 'A', Lit 'Z']), Star Any])
        , Let [] (Plus (Choice [Lit 'T', Lit 'W', Lit 'M', Lit 'L']))
        , Let [] (Plus (Choice [Lit 'W', Lit 'R', Lit 'Z', Lit 'G']))
        , Let [] (Plus (Choice [SSeq [Lit 'S', Lit 'A'], Lit 'T', SSeq [Lit 'P', Lit 'N'], Lit 'X']))
        , Let [] (Plus (Choice [Lit 'O', Lit 'N', Lit 'D', Lit 'H']))
        ]

    testCase "Size 5 Hexagon" "ABCEFGHIJKLMNOPQRST"
        [ Let [] (S (SSeq [Lit 'A', Lit 'B', Lit 'C']))
        , Let [] (S (SSeq [Lit 'E', Lit 'F', Lit 'G', Lit 'H']))
        , Let [] (S (SSeq [Lit 'I', Lit 'J', Lit 'K', Lit 'L', Lit 'M']))
        , Let [] (S (SSeq [Lit 'N', Lit 'O', Lit 'P', Lit 'Q']))
        , Let [] (S (SSeq [Lit 'R', Lit 'S', Lit 'T']))
        , Let [] (S (SSeq [Lit 'I', Lit 'N', Lit 'R']))
        , Let [] (S (SSeq [Lit 'E', Lit 'J', Lit 'O', Lit 'S']))
        , Let [] (S (SSeq [Lit 'A', Lit 'F', Lit 'K', Lit 'P', Lit 'T']))
        , Let [] (S (SSeq [Lit 'B', Lit 'G', Lit 'L', Lit 'Q']))
        , Let [] (S (SSeq [Lit 'C', Lit 'H', Lit 'M']))
        , Let [] (S (SSeq [Lit 'T', Lit 'Q', Lit 'M']))
        , Let [] (S (SSeq [Lit 'S', Lit 'P', Lit 'L', Lit 'H']))
        , Let [] (S (SSeq [Lit 'R', Lit 'O', Lit 'K', Lit 'G', Lit 'C']))
        , Let [] (S (SSeq [Lit 'N', Lit 'J', Lit 'F', Lit 'B']))
        , Let [] (S (SSeq [Lit 'I', Lit 'E', Lit 'A']))
        ]
        
    putStrLn "\n=== All Challenge 5 tests complete ===\n"

testCase :: String -> String -> [LRegExp] -> IO ()
testCase description expected constraints = do
    let solution = solveRegexle constraints
    let status = if solution == expected then "PASS" else "FAIL"
    putStrLn $ "Test: " ++ description
    putStrLn $ "Expected: " ++ show expected
    putStrLn $ "Output:   " ++ show solution
    putStrLn $ "Status:   " ++ status
    putStrLn ""
