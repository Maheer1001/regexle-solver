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
        
    testCase "Size 5 Hexagon with choices (bundled sample puzzle)" "POUELUCKWOOLESOEKLY"
        [ Let [] (S (SSeq [Choice [SSeq [Lit 'P',Lit 'O'],SSeq [Lit 'B',Lit 'A']],Choice [Lit 'U',Lit 'T']]))
        , Let [] (S (SSeq [Lit 'E',Lit 'L',Choice [SSeq [Lit 'U',Lit 'C'],SSeq [Lit 'G',Lit 'N']]]))
        , Let [] (Plus (Choice [Lit 'K',SSeq [Lit 'Y',Lit 'P',Lit 'T',Lit 'Y'],SSeq [Lit 'W',Lit 'O',Lit 'O'],Lit 'L']))
        , Let [] (Plus (Choice [SSeq [Opt 'S',Lit 'O'],Lit 'E']))
        , Let [] (Plus (Choice [SSeq [Opt 'L',Lit 'Y'],Lit 'B',Lit 'K']))
        , Let [] (Plus (Choice [Lit 'A',SSeq [Lit 'Z',Lit 'D'],SSeq [Opt 'E',Lit 'K']]))
        , Let [] (Plus (Choice [Lit 'K',SSeq [Lit 'Q',Lit 'X'],SSeq [Lit 'E',Lit 'W'],SSeq [Lit 'S',Lit 'L']]))
        , Let [] (Seq [Plus (Choice [Lit 'P',Lit 'L',Lit 'M']),S (SSeq [Lit 'O',Lit 'O',Lit 'Y'])])
        , Let [] (Plus (Choice [Lit 'E',SSeq [Opt 'U',Lit 'O'],SSeq [Lit 'X',Lit 'X']]))
        , Let [] (Plus (Choice [SSeq [Lit 'Y',Lit 'F'],SSeq [Opt 'C',Lit 'L'],Lit 'U']))
        , Let [] (Plus (Choice [Lit 'U',Lit 'Y',SSeq [Lit 'E',Opt 'L']]))
        , Let [] (Plus (Choice [SSeq [Lit 'G',Lit 'C',Lit 'M'],SSeq [Lit 'O',Lit 'C'],Lit 'Q',SSeq [Lit 'L',Lit 'O']]))
        , Let [] (Plus (Choice [SSeq [Lit 'I',Lit 'T',Lit 'N',Lit 'H'],Lit 'E',SSeq [Lit 'K',Lit 'S'],SSeq [Lit 'O',Lit 'U',Lit 'U']]))
        , Let [] (Plus (Choice [SSeq [Lit 'E',Lit 'W'],SSeq [Lit 'L',Lit 'O'],Lit 'P',SSeq [Lit 'R',Lit 'R',Lit 'H']]))
        , Let [] (Plus (Choice [SSeq [Opt 'E',Lit 'P'],Lit 'C',Lit 'K']))
        ]

    -- Unsolvable input: contradictory clues leave a cell with no candidates
    testCase "Unsolvable size 1 (A+ vs B+) gives an empty answer" ""
        [Let [] (Plus (Lit 'A')), Let [] (Plus (Lit 'B')), Let [] (Plus (Lit 'A'))]

    -- Constraint propagation internals
    check "cellRowStructure for n = 1, 3, 5"
        [[1], [2, 3, 2], [3, 4, 5, 4, 3]] (map cellRowStructure [1, 3, 5])
    check "generatePossibilities keeps only strings of the row's exact length"
        [["AA", "AB", "BA", "BB"]]
        (generatePossibilities [Let [] (Plus (Choice [Lit 'A', Lit 'B']))] [2])
    check "analyseCellPositions: per-cell candidates, '.' widened, later orientations reversed"
        [["A", "BC"], [['A'..'Z'], "X"], ["R", "Q"]]
        (analyseCellPositions [["AB", "AC"], ["X."], ["QR"]])
    check "expandWildcards widens '.' to A-Z and leaves letters alone"
        (['A'..'Z'], "AB") (expandWildcards "A.", expandWildcards "AB")
    check "rotateHexgrid turns candidate grids by 120 degrees"
        ["FC", "GDA", "EB"] (rotateHexgrid ["AB", "CDE", "FG"] [2, 3, 2])
    check "rotateNTimes 3 returns the original grid"
        [["A", "B"], ["C", "D", "E"], ["F", "G"]]
        (rotateNTimes (map (map (:[])) ["AB", "CDE", "FG"]) [2, 3, 2] 3)

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

check :: (Eq a, Show a) => String -> a -> a -> IO ()
check description expected result = do
    let status = if result == expected then "PASS" else "FAIL"
    putStrLn $ "Test: " ++ description
    putStrLn $ "Expected: " ++ show expected
    putStrLn $ "Output:   " ++ show result
    putStrLn $ "Status:   " ++ status
    putStrLn ""
