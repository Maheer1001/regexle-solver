{-
Author: Maheer Mirza
Student ID: mmm3g24
Copyright: University of Southampton 2025
Description: Tests for Haskell Coursework
-}

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

-- ============================================================================
-- TEST SUITE 4
-- ============================================================================

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

-- ============================================================================
-- TEST SUITE 6
-- ============================================================================

main :: IO ()
main = do
    putStrLn "=== Challenge 6: Lambda Calculus with Exceptions ===\n"
    
    testCase "Identity function (value)" 10 
        (Abs 0 (Var 0))
        (0, 0)
    
    testCase "Apply identity to identity" 10
        (App (Abs 1 (Var 1)) (Abs 1 (Var 1)))
        (1, 1)
    
    testCase "Nested application" 10
        (App (App (Abs 1 (Abs 2 (Var 1))) (Var 3)) (Var 4))
        (2, 2)
    
    testCase "Exception alone" 10
        Exn
        (0, 0)
    
    testCase "Simple catch with exception" 10
        (Catch (Abs 1 (Var 1)) Exn)
        (1, 1)
    
    testCase "Simple catch with lambda" 10
        (Catch (Abs 1 (Var 1)) (Abs 2 (Var 2)))
        (1, 1)
    
    testCase "Exception propagation: exn e" 10
        (App Exn (Abs 1 (Var 1)))
        (1, 1)
    
    testCase "Exception propagation: e exn" 10
        (App (Abs 1 (Var 1)) Exn)
        (1, 1)
    
    testCase "Full example from spec" 4
        (Catch (App (Abs 1 (Var 1)) (Abs 1 (Var 1))) 
                (App (Abs 3 (Abs 2 (Var 2))) 
                    (App (Abs 4 (App Exn (Abs 2 (Var 2)))) 
                        (Abs 1 (Var 1)))))
        (4, 3)
    
    testCase "CBV evaluates argument first" 10
        (App (Abs 1 (Var 1)) (App (Abs 2 (Var 2)) (Var 3)))
        (2, 2)
    
    testCase "Unused argument (CBN advantage)" 10
        (App (Abs 1 (Abs 2 (Var 2))) (App (Abs 3 (Var 3)) (Var 4)))
        (2, 1)
    
    testCase "Exception in unused position" 10
        (App (Abs 1 (Abs 2 (Var 2))) (App Exn (Var 3)))
        (2, 1)
    
    testCase "Bound limit test" 2
        (App (App (Abs 1 (Abs 2 (Var 1))) (Var 3)) (Var 4))
        (2, 2)
    
    testCase "Exception in handler" 10
        (Catch (App Exn (Var 1)) (Abs 2 (Var 2)))
        (2, 2)
    
    putStrLn "\n=== All Challenge 6 tests complete ===\n"

testCase :: String -> Int -> Exp -> (Int, Int) -> IO ()
testCase description bound expr expected = do
    let result = countReductions bound expr
    let status = if result == expected then "PASS" else "FAIL"
    putStrLn $ "Test: " ++ description
    putStrLn $ "Bound: " ++ show bound
    putStrLn $ "Expression: " ++ show expr
    putStrLn $ "Expected: " ++ show expected
    putStrLn $ "Output:   " ++ show result
    putStrLn $ "Status:   " ++ status
    putStrLn ""