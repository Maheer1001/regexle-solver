{-
Author: Maheer Mirza
Student ID: mmm3g24
Copyright: University of Southampton 2025
Description: Hexagonal Regexle validator
-}

import Data.Maybe (fromMaybe)

-- Data type definitions
data LRegExp = Let [(Int,RegExp)] RegExp
    deriving (Eq,Show,Read)

data RegExp = S SRegExp | Star SRegExp | Plus SRegExp | Seq [RegExp]
    deriving (Eq,Show,Read)

data SRegExp = Empty | Lit Char | Opt Char | Any | OptAny | SSeq [SRegExp] | Choice [SRegExp] | Var Int
    deriving (Eq,Show,Read)

-- | Check if a hexagonal grid solution matches all regular expression constraints
-- Returns a list of booleans indicating whether each constraint is satisfied
checkRegexle :: String -> [LRegExp] -> [Bool]
checkRegexle answer constraints =
    let pairs = constraintPairing answer constraints
    in map (uncurry matchesLRegExp) pairs

-- Check if a string matches an LRegExp (considering wildcards)
matchesLRegExp :: String -> LRegExp -> Bool
matchesLRegExp str lregexp = any (wildcardMatch str) (boundedLang (length str) lregexp)

-- Match a string against a pattern containing wildcards (.)
wildcardMatch :: String -> String -> Bool
wildcardMatch [] [] = True
wildcardMatch (c:cs) ('.':ps) = wildcardMatch cs ps
wildcardMatch (c:cs) (p:ps) = c == p && wildcardMatch cs ps
wildcardMatch _ _ = False

-- Number of orientations in hexagonal grid (horizontal, diagonal left, diagonal right)
orientations :: Int
orientations = 3

-- Calculate side length from number of constraints
sideLengths :: [LRegExp] -> Int
sideLengths constraints = length constraints `div` orientations

-- Generate the row structure for a hexagonal grid of given side length
-- For size n, rows have pattern: [n, n+1, ..., 2n-1, ..., n+1, n]
cellRowStructure :: Int -> [Int]
cellRowStructure 1 = [1]
cellRowStructure n =
    let left = [(n+1) `div` 2..n]
        right = tail (reverse left)
    in left ++ right

-- Format a flat string into hexagonal grid structure based on row lengths
hexgridFormat :: [a] -> [Int] -> [[a]]
hexgridFormat = go []
    where
        go acc [] [] = []
        go acc [] [0] = [reverse acc]
        go acc [] (y:ys) = []
        go acc (x:xs) [] = []
        go acc (x:xs) (0:ys) = reverse acc : go [] (x:xs) ys
        go acc (x:xs) (y:ys) = go (x:acc) xs (y-1:ys)

-- Rotate hexagonal grid by 120 degrees to extract rows in different orientation
rotateHexgrid :: [String] -> [Int] -> [String]
rotateHexgrid _ [] = []
rotateHexgrid input (r:rs) =
    let (updated, newRow) = consumeHeads input r
    in newRow : rotateHexgrid updated rs

-- Consume first characters from specific rows to form new row after rotation
consumeHeads :: [String] -> Int -> ([String],String)
consumeHeads input n =
    let
        nonEmptyIndices = [i | (i, row) <- zip [0..] input, not (null row)]
        rowsToConsume = take n (reverse nonEmptyIndices)
        heads = map (\i -> head (input !! i)) rowsToConsume
        updated = [if i `elem` rowsToConsume then tail row else row
                    | (i, row) <- zip [0..] input]
    in (updated, heads)

-- Pair each row (in all three orientations) with its corresponding constraint
constraintPairing :: String -> [LRegExp] -> [(String,LRegExp)]
constraintPairing answer constraints =
    let sides = sideLengths constraints
        rowStructure = cellRowStructure sides
        grid = hexgridFormat answer rowStructure
        grid2 = rotateHexgrid grid rowStructure
        grid3 = rotateHexgrid grid2 rowStructure
        answerSet = grid ++ map reverse grid2 ++ map reverse grid3
    in zip answerSet constraints

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