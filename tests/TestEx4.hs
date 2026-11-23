-- Tests for challenge 4. Run from the repo root: runghc -i. tests/TestEx4.hs

import Ex4

main :: IO ()
main = do
    putStrLn "=== Challenge 4: Hexagonal Regexle Tests ===\n"
    
    testCase "Size 1 Hexagon (Valid)" "X" 
        [Let [] (Star Any), Let [] (Star Any), Let [] (Star Any)]
        [True, True, True]
    
    testCase "Size 1 Hexagon (Invalid)" "Y" 
        [Let [] (Plus (Lit 'X')), Let [] (Plus (Lit 'X')), Let [] (Plus (Lit 'X'))]
        [False, False, False]
    
    testCase "Size 3 Hexagon (Invalid)" "ABCDEFG" 
        [ Let [] (Star Any)
        , Let [] (Plus (Lit 'C'))
        , Let [] (Plus (Lit 'F'))
        , Let [] (Plus (Lit 'F'))
        , Let [] (Plus (Lit 'G'))
        , Let [] (Plus (Lit 'D'))
        , Let [] (Plus (Lit 'E'))
        , Let [] (Plus (Lit 'G'))
        , Let [] (Star Any)
        ]
        [True,False,False,False,False,False,False,False,True]
    
    testCase "Size 3 Hexagon (Invalid)" "ABCDEFG" 
        [ Let [] (Plus (Lit 'Z'))
        , Let [] (Plus (Lit 'A'))
        , Let [] (Plus (Lit 'B'))
        , Let [] (Star Any)
        , Let [] (Plus (Lit 'C'))
        , Let [] (Plus (Lit 'D'))
        , Let [] (Star Any)
        , Let [] (Plus (Lit 'E'))
        , Let [] (Plus (Lit 'F'))
        ]
        [False, False, False, True, False, False, True, False, False]
    
    testCase "Size 5 Hexagon (Valid)" "POUELUCKWOOLESOEKLY"
        [ Let [] (Star Any)
        , Let [] (Plus (Choice [SSeq [Lit 'E',Lit 'L'],SSeq [Lit 'G',Lit 'N'],SSeq [Lit 'U',Lit 'C'],Lit 'M']))
        , Let [] (Plus (Choice [Lit 'K',SSeq [Lit 'Y',Lit 'P',Lit 'T',Lit 'Y'],SSeq [Lit 'W',Lit 'O',Lit 'O'],Lit 'L']))
        , Let [] (Plus (Choice [SSeq [Opt 'S',Lit 'O'],Lit 'E']))
        , Let [] (Plus (Choice [SSeq [Opt 'L',Lit 'Y'],Lit 'B',Lit 'K']))
        , Let [] (Plus (Choice [Lit 'A',SSeq [Lit 'Z',Lit 'D'],SSeq [Opt 'E',Lit 'K']]))
        , Let [] (Plus (Choice [Lit 'K',SSeq [Lit 'Q',Lit 'X'],SSeq [Lit 'E',Lit 'W'],SSeq [Lit 'S',Lit 'L']]))
        , Let [] (Seq [Star Any,S (SSeq [Lit 'O',Lit 'O',Lit 'Y']), Star Any])
        , Let [] (Plus (Choice [Lit 'E',SSeq [Opt 'U',Lit 'O'],SSeq [Lit 'X',Lit 'X']]))
        , Let [] (Plus (Choice [SSeq [Lit 'Y',Lit 'F'],SSeq [Opt 'C',Lit 'L'],Lit 'U']))
        , Let [] (Plus (Choice [Lit 'U',Lit 'Y',SSeq [Lit 'E',Opt 'L']]))
        , Let [] (Plus (Choice [SSeq [Lit 'G',Lit 'C',Lit 'M'],SSeq [Lit 'O',Lit 'C'],Lit 'Q',SSeq [Lit 'L',Lit 'O']]))
        , Let [] (Plus (Choice [SSeq [Lit 'I',Lit 'T',Lit 'N',Lit 'H'],Lit 'E',SSeq [Lit 'K',Lit 'S'],SSeq [Lit 'O',Lit 'U',Lit 'U']]))
        , Let [] (Plus (Choice [SSeq [Lit 'E',Lit 'W'],SSeq [Lit 'L',Lit 'O'],Lit 'P',SSeq [Lit 'R',Lit 'R',Lit 'H']]))
        , Let [] (Plus (Choice [SSeq [Opt 'E',Lit 'P'],Lit 'C',Lit 'K']))
        ]
        [True,True,True,True,True,True,True,True,True,True,True,True,True,True,True]
    
    testCase "Size 5 Hexagon 2 (Valid)" "ABCEFGHIJKLMNOPQRST"
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
        [True,True,True,True,True,True,True,True,True,True,True,True,True,True,True]
    
    -- Grid helpers the checker is built on
    check "hexgridFormat splits a size 3 answer into 2-3-2 rows"
        ["AB", "CDE", "FG"] (hexgridFormat "ABCDEFG" [2, 3, 2])
    check "rotateHexgrid turns a size 3 grid by 120 degrees"
        ["FC", "GDA", "EB"] (rotateHexgrid ["AB", "CDE", "FG"] [2, 3, 2])
    check "Three rotations return the original grid"
        ["AB", "CDE", "FG"]
        (iterate (`rotateHexgrid` [2, 3, 2]) ["AB", "CDE", "FG"] !! 3)
    check "constraintPairing reads rows in all three orientations"
        ["HT", "DAW", "SZ", "DS", "HAZ", "TW", "ZW", "SAT", "DH"]
        (map fst (constraintPairing "HTDAWSZ" (replicate 9 (Let [] (S Empty)))))
    check "wildcardMatch: '.' matches any one character, lengths must agree"
        [True, False, False] [wildcardMatch "AXB" "A.B", wildcardMatch "AXB" "A.C", wildcardMatch "AB" "A.B"]
    check "matchesLRegExp respects the exact row length"
        [True, False] [matchesLRegExp "AAA" (Let [] (Plus (Lit 'A'))), matchesLRegExp "AAB" (Let [] (Plus (Lit 'A')))]

    putStrLn "\n=== All Challenge 4 tests complete ===\n"

testCase :: String -> String -> [LRegExp] -> [Bool] -> IO ()
testCase description answer constraints expected = do
    let result = checkRegexle answer constraints
    let status = if result == expected then "PASS" else "FAIL"
    putStrLn $ "Test: " ++ description
    putStrLn $ "Expected: " ++ show expected
    putStrLn $ "Output:   " ++ show result
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
