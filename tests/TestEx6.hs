-- Tests for challenge 6. Run from the repo root: runghc -i. tests/TestEx6.hs

import Ex6

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
